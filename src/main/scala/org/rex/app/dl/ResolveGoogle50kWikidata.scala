package org.rex.app.dl

import java.io._
import java.util.concurrent.TimeUnit
import java.util.zip.GZIPInputStream

import scala.collection.GenTraversableOnce
import scala.concurrent.duration.Duration
import scala.io.Source
import scala.util.{ Failure, Success, Try }

object ResolveGoogle50kWikidata extends App {

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
      println("" +
        s"Google 50k Relations KB:    ${config.googleDir}\n" +
        s"Freebase2Wikidata ID map:   ${config.freebase2wikidataId}\n" +
        s"Wikidata KB dump:           ${config.wikidataDump}\n" +
        s"Output, simplified triples: ${config.triplesKbOut}\n"
      )

      // read and parse json of each Google 50K relation lines
      // get query & answer Freebase IDs, store everything into one Set[FreebaseId]
      val startFbGKb = System.currentTimeMillis()
      val freebaseIdGoogleKb =
        GoogleStuff.loadFreebaseIdGoogleKb(
          config.googleDir
        )
      val endFbGKb = System.currentTimeMillis()
      LoadUtils.printTime("Finished loading Google KB in", startFbGKb, endFbGKb)

      // read and parse the freebase -> wikidata ID file
      // construct a mapping of type Map[FreebaseId, WikidataId]
      val startFb2Wd = System.currentTimeMillis()
      val fb2wd =
        Freebase2WikidataStuff.loadFreebase2WikidataIdMap(
          freebaseIdGoogleKb,
          config.freebase2wikidataId
        )
      val endFb2Wd = System.currentTimeMillis()
      LoadUtils.printTime("Finished loading Freebase ID -> Wikidata ID mapping in", startFb2Wd, endFb2Wd)

      // // read and parse the Wikidata dump file
      // extracting all text mentions for each Wikidata ID that we're interested in
      val startWText = System.currentTimeMillis()
      val wikidataId2textMentions =
        WikidataDumpStuff.loadWikidataTextMentions(
          fb2wd,
          config.wikidataDump
        )
      val endWText = System.currentTimeMillis()
      LoadUtils.printTime("Finished loading Wikidata Text Mentions in", startWText, endWText)

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

    case None =>
      System.exit(1)
  }

  type FreebaseId = String
  type WikidataId = String
  type Relation = String
  type Mention = String

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

  import ResolveGoogle50kWikidata.{ FreebaseId, WikidataId, Mention }

  import rapture.json.jsonBackends.jackson._
  import rapture.json._

  @inline def jsonParse(idsOfInterest: Set[WikidataId])(s: String): Option[(WikidataId, Seq[Mention])] =
    Try(Json.parse(s)) match {

      case Success(j) =>
        val id = j.id.as[WikidataId]
        if (idsOfInterest contains id)
          Some {
            val labelMentions =
              Seq(j.labels.en.value.as[Mention], j.labels.simple.value.as[Mention])

            val aliasMentions =
              j.aliases.en.as[Seq[String]]
                .map(x => Json.parse(x).value.as[Mention])

            (id, labelMentions ++ aliasMentions)
          }
        else
          None

      case Failure(e) =>
        e.printStackTrace()
        println(s"[WikidataDumpStuff] ERROR parsing:\n\n$s\n\n")
        None
    }

  type WikidataTextMentions = Map[WikidataId, Seq[Mention]]

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

  import org.rex.app.dl.Freebase2WikidataStuff.Freebase2WikidataMap
  import org.rex.app.dl.GoogleStuff.FbKnowledgeBase
  import org.rex.app.dl.WikidataDumpStuff.WikidataTextMentions

  @inline def apply(
    freebaseIdGoogleKb: FbKnowledgeBase,
    fb2wd: Freebase2WikidataMap,
    wikidataId2textMentions: WikidataTextMentions,
    triplesKbOut: File) = {
    val w = new BufferedWriter(new FileWriter(triplesKbOut))
    try {
      freebaseIdGoogleKb
        .foreach {
          case (subFb, objFbRelMap) =>

            val subWd = fb2wd(subFb)
            val subMentions = wikidataId2textMentions(subWd)

            objFbRelMap
              .foreach {
                case (objFb, relation) =>

                  val objWd = fb2wd(objFb)
                  val objMentions = wikidataId2textMentions(objWd)

                  subMentions
                    .foreach { sMention =>
                      objMentions
                        .foreach { oMention =>
                          w.write(s"$sMention\t$oMention\t$relation\n")
                        }
                    }
              }
        }
    } finally {
      w.close()
    }
  }
}

object Freebase2WikidataStuff {

  import ResolveGoogle50kWikidata.{ FreebaseId, WikidataId }

  type Freebase2WikidataMap = Map[FreebaseId, WikidataId]

  @inline def loadFreebase2WikidataIdMap(fbkb: GoogleStuff.FbKnowledgeBase, fb2wdFi: File): Freebase2WikidataMap = {
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

  @inline def freebaseIdsOfInterest(fbkb: GoogleStuff.FbKnowledgeBase): Set[FreebaseId] =
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
    (extractId(bits(0)), extractId(bits(2)))
  }

  @inline def extractId(s: String) =
    s.substring(s.lastIndexOf("/") + 1, s.lastIndexOf(">"))
}

object GoogleStuff {

  import ResolveGoogle50kWikidata.{ FreebaseId, Relation }

  import rapture.json.jsonBackends.jackson._
  import rapture.json._

  type FbKnowledgeBase = Map[FreebaseId, Map[FreebaseId, Set[Relation]]]

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
                  e.printStackTrace()
                  println(s"[loadFreebaseIdGoogleKb] ERROR (${e.getMessage}) offending line\n\n$l\n\n")
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