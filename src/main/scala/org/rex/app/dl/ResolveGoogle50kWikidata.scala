package org.rex.app.dl

import java.io._
import java.util.concurrent.TimeUnit
import java.util.zip.GZIPInputStream

import scala.collection.GenTraversableOnce
import scala.concurrent.duration.Duration
import scala.io.Source
import scala.util.{ Try, Failure, Success }

object ResolveGoogle50kWikidata extends App {

  type FreebaseId = String
  type WikidataId = String
  type Relation = String
  type Mention = String

  type FbKnowledgeBase = Map[FreebaseId, Map[FreebaseId, Set[Relation]]]
  type Freebase2WikidataMap = Map[FreebaseId, WikidataId]
  type WikidataTextMentions = Map[WikidataId, Seq[Mention]]

  private[this] def apply(config: Config, k: Int = 10): Unit = {
    println("" +
      s"Google 50k Relations KB:    ${config.googleDir}\n" +
      s"Freebase2Wikidata ID map:   ${config.freebase2wikidataId}\n" +
      s"Wikidata KB dump:           ${config.wikidataDump}\n" +
      s"Output, simplified triples: ${config.triplesKbOut}\n"
    )

    // read and parse json of each Google 50K relation lines
    // get query & answer Freebase IDs, store everything into one Set[FreebaseId]
    val startFbGKb = System.currentTimeMillis()
    val freebaseIdGoogleKb: FbKnowledgeBase =
      GoogleStuff.loadFreebaseIdGoogleKb(
        config.googleDir
      )
    val endFbGKb = System.currentTimeMillis()
    LoadUtils.printTime("Finished loading Google KB in", startFbGKb, endFbGKb)
    println(s"Sample of $k Google 50K Knowledgebase Entries")
    freebaseIdGoogleKb
      .take(k)
      .foreach(println)

    // read and parse the freebase -> wikidata ID file
    // construct a mapping of type Map[FreebaseId, WikidataId]
    val startFb2Wd = System.currentTimeMillis()
    val fb2wd: Freebase2WikidataMap =
      Freebase2WikidataStuff.loadFreebase2WikidataIdMap(
        freebaseIdGoogleKb,
        config.freebase2wikidataId
      )
    val endFb2Wd = System.currentTimeMillis()
    LoadUtils.printTime("Finished loading Freebase ID -> Wikidata ID mapping in", startFb2Wd, endFb2Wd)
    println(s"Sample of $k Freebase ID => WikidataID mappings")
    fb2wd
      .take(k)
      .foreach(println)

    // // read and parse the Wikidata dump file
    // extracting all text mentions for each Wikidata ID that we're interested in
    val startWText = System.currentTimeMillis()
    val wikidataId2textMentions: WikidataTextMentions =
      WikidataDumpStuff.loadWikidataTextMentions(
        fb2wd,
        config.wikidataDump
      )
    val endWText = System.currentTimeMillis()
    LoadUtils.printTime("Finished loading Wikidata Text Mentions in", startWText, endWText)
    println(s"Sample of $k WikidataID text mentions")
    wikidataId2textMentions
      .take(k)
      .foreach(println)

    // go through google KB mapping, translating Freebase ID to Wikidata ID,
    // grabbing the relevant mentions, and then writing them all to disk
    val startTOut = System.currentTimeMillis()
    OutputSimplifiedTriples(
      freebaseIdGoogleKb,
      fb2wd,
      wikidataId2textMentions,
      config.triplesKbOut
    )
    val endTOut = System.currentTimeMillis()
    LoadUtils.printTime("Finished writing simplified relation triples in", startTOut, endTOut)
  }

  case class Config(
    googleDir: File,
    freebase2wikidataId: File,
    wikidataDump: File,
    triplesKbOut: File)

  new scopt.OptionParser[Config]("relation-extraction-learning-main") {
    head("Make KB from Google 50k relations and Wikidata names")

    help("help")
      .text("prints this usage text")

    opt[File]("google")
      .required()
      .abbr("g")
      .valueName("<directory>")
      .action { (g, c) => c.copy(googleDir = g) }
      .text("The Google 50k Relation Knowledge Base Directory")

    opt[File]("freebase2wikidata")
      .required()
      .abbr("f")
      .valueName("<file>")
      .action { (f2w, c) => c.copy(freebase2wikidataId = f2w) }
      .text("The Mapping from Freebase to Wikidata IDs")

    opt[File]("wikidata")
      .required()
      .abbr("w")
      .valueName("<file>")
      .action { (w, c) => c.copy(wikidataDump = w) }
      .text("The Complete Wikidata Dump")

    opt[File]("triplesOut")
      .required()
      .abbr("t")
      .valueName("<file>")
      .action { (t, c) => c.copy(triplesKbOut = t) }
      .text("Output: where to write the simplified KB triples")

    checkConfig { c =>
      if (c.freebase2wikidataId != null && c.googleDir != null && c.triplesKbOut != null && c.wikidataDump != null)
        if (c.freebase2wikidataId.isFile)
          if (c.googleDir.isDirectory)
            if (c.wikidataDump.isFile)
              if (!c.triplesKbOut.exists())
                success
              else
                failure(s"Output already exists: ${c.triplesKbOut}")
            else
              failure(s"Wikidata dump file doesn't exist: ${c.wikidataDump}")
          else
            failure(s"Google directory doesn't exist: ${c.googleDir}")
        else
          failure(s"Freebase -> Wikidata ID mapping file doesn't exist: ${c.freebase2wikidataId}")
      else
        failure("Not all arguments supplied.")
    }

  }.parse(args, Config(null, null, null, null)) match {

    case Some(config) =>
      apply(config)

    case None =>
      System.exit(1)
  }

}

object LoadUtils {

  @inline def printTime(msg: String, start: Long, end: Long): Unit =
    println(s"$msg ${Duration(end - start, TimeUnit.MILLISECONDS).toSeconds} seconds (${end - start} ms)")

  @inline def asInputStream(f: File): Option[InputStream] =
    if (f.isFile)
      Try {
        if (f.getName.endsWith(".gz"))
          new GZIPInputStream(new BufferedInputStream(new FileInputStream(f)))
        else
          new BufferedInputStream(new FileInputStream(f))
      } toOption
    else
      None
}

object WikidataDumpStuff {

  import ResolveGoogle50kWikidata.{ FreebaseId, WikidataId, Mention, WikidataTextMentions }

  import rapture.json.jsonBackends.jackson._
  import rapture.json._

  @inline private[this] def obtain(attempts: Try[Seq[Mention]]*): Seq[Mention] =
    attempts
      .flatMap(_.toOption)
      .foldLeft(Seq.empty[Mention])(_ ++ _)

  def jsonParse(idsOfInterest: Set[WikidataId])(s: String): Option[(WikidataId, Seq[Mention])] =
    Try(Json.parse(s)) match {

      case Success(j) =>
        val id: WikidataId = j.id.as[WikidataId]
        if (idsOfInterest contains id) {

          val mentions =
            obtain(
              Try(Seq(j.labels.en.value.as[Mention])),
              Try(Seq(j.labels.simple.value.as[Mention])),
              Try {
                j.aliases.en.as[Seq[String]]
                  .map(x => Json.parse(x).value.as[Mention])
              }
            )
          if (mentions nonEmpty)
            Some((id, mentions))
          else
            None
        } else
          None

      case Failure(e) =>
        println(s"[WikidataDumpStuff] *recovered, skip* ERROR parsing:\n\n$s\n\n")
        e.printStackTrace()
        println("[WikidataDumpStuff] /////////////////////////////////")
        None
    }

  @inline def loadWikidataTextMentions(
    fb2wd: Map[FreebaseId, WikidataId],
    wikdataDumpFi: File): WikidataTextMentions = {

    val parser: String => GenTraversableOnce[(String, Seq[String])] =
      WikidataDumpStuff.jsonParse(fb2wd.values.toSet) _

    Source.fromInputStream(LoadUtils.asInputStream(wikdataDumpFi).get)
      .getLines()
      // don't care about array nature of the file, only processing
      // each element individually
      .dropWhile(x => x == "[" || x == "]")
      // remove the ending ",", otherwise JSON parsing will choke
      // (don't want to parse json as one big old array)
      .map(x => x.substring(0, x.length - 1))
      .flatMap(parser)
      .toMap
  }

}

object OutputSimplifiedTriples {

  import org.rex.app.dl.ResolveGoogle50kWikidata.{ Freebase2WikidataMap, FbKnowledgeBase, WikidataTextMentions }

  @inline def apply(
    freebaseIdGoogleKb: FbKnowledgeBase,
    fb2wd: Freebase2WikidataMap,
    wikidataId2textMentions: WikidataTextMentions,
    triplesKbOut: File) = {
    //
    val w = new BufferedWriter(new FileWriter(triplesKbOut))
    try {
      freebaseIdGoogleKb
        .foreach {
          case (subFb, objFbRelMap) =>
            fb2wd.get(subFb) match {

              case Some(subWd) =>
                val subMentions = wikidataId2textMentions(subWd)
                objFbRelMap
                  .foreach {
                    case (objFb, relation) =>
                      fb2wd.get(objFb) match {

                        case Some(objWd) =>
                          val objMentions = wikidataId2textMentions(objWd)
                          subMentions
                            .foreach { sMention =>
                              objMentions
                                .foreach { oMention =>
                                  w.write(s"$sMention\t$oMention\t$relation\n")
                                }
                            }

                        case None =>
                          println(s"[OutputSimplifiedTriples] *recovered, skip* ERROR, no Wikidata ID for Freebase ID (object): $objFb")
                      }
                  }

              case None =>
                println(s"[OutputSimplifiedTriples] *recovered, skip* ERROR, no Wikidata ID for Freebase ID (subject): $subFb")
            }
        }
    } finally {
      w.close()
    }
  }
}

object Freebase2WikidataStuff {

  import ResolveGoogle50kWikidata.{ FreebaseId, WikidataId, FbKnowledgeBase, Freebase2WikidataMap }

  @inline def loadFreebase2WikidataIdMap(fbkb: FbKnowledgeBase, fb2wdFi: File): Freebase2WikidataMap = {
    // get a set of all freebase IDs from the Google KB
    val interesting = freebaseIdsOfInterest(fbkb)

    // translate Freebase IDs into Wikidata IDs
    // only keep the ones we're interested in
    Source.fromInputStream(LoadUtils.asInputStream(fb2wdFi).get)
      .getLines()
      // ignore commments and empty lines
      .dropWhile(x => x.startsWith("#") || x.trim.isEmpty)
      .map(Freebase2WikidataStuff.extractBothIds)
      .filter { case (freebaseId, _) => interesting contains freebaseId }
      .toMap
  }

  @inline def freebaseIdsOfInterest(fbkb: FbKnowledgeBase): Set[FreebaseId] =
    fbkb
      .foldLeft(Set.empty[FreebaseId]) {
        case (idSet, (sub, objMap)) =>
          objMap.foldLeft(idSet) {
            case (ids, (obj, _)) =>
              ids + obj
          } + sub
      }

  @inline def extractBothIds(s: String): (FreebaseId, WikidataId) = {
    val bits = s.replaceAll(" \\.", "").split("\\t")
    (
      s"/${extractId(bits(0)).replaceAll("\\.", "/")}",
      extractId(bits(2))
    )
  }

  @inline def extractId(s: String) =
    s.substring(s.lastIndexOf("/") + 1, s.lastIndexOf(">"))
}

object GoogleStuff {

  import ResolveGoogle50kWikidata.{ FreebaseId, Relation, FbKnowledgeBase }

  import rapture.json.jsonBackends.jackson._
  import rapture.json._

  @inline def loadFreebaseIdGoogleKb(googleDir: File): FbKnowledgeBase =
    googleDir
      .listFiles
      .filter(_.getName.endsWith(".json"))
      .filter(_.getName.startsWith("fixed_"))
      .flatMap(f =>
        LoadUtils.asInputStream(f)
          .map(is => {
            val name = GoogleStuff.relationFromName(f)
            println(s"Reading relation KB: $name")
            (name, is)
          })
      )
      .foldLeft(Map.empty[FreebaseId, Map[FreebaseId, Set[Relation]]]) {
        case (knowledgebase, (relation, is)) =>
          Source.fromInputStream(is)
            .getLines()
            .flatMap(l =>
              GoogleStuff.idsFromJsonLine(l) match {
                case Success(x) =>
                  Some(x)
                case Failure(e) =>
                  println(s"[loadFreebaseIdGoogleKb] *recovered, skipped* ERROR, offending line\n\n$l\n\n")
                  e.printStackTrace()
                  println("[loadFreebaseIdGoogleKb] ///////////////////////////////////////////")
                  None
              }
            )
            .foldLeft(knowledgebase) {

              case (kb, (sub, obj)) =>

                if (kb contains sub) {
                  (kb - sub) + (sub -> (
                    if (kb(sub) contains obj) {
                      (kb(sub) - obj) + (obj -> (kb(sub)(obj) + relation))
                    } else
                      kb(sub) + (obj -> Set(relation))
                  ))
                } else
                  kb + (sub -> Map(obj -> Set(relation)))
            }
      }

  @inline def relationFromName(f: File): String = {
    val s = f.getName
    s.substring(s.indexOf("-") + 1, s.indexOf(".json"))
  }

  @inline def idsFromJsonLine(s: String): Try[(FreebaseId, FreebaseId)] =
    Try {
      val j = Json.parse(s)
      (j.sub.as[FreebaseId], j.obj.as[FreebaseId])
    }
}