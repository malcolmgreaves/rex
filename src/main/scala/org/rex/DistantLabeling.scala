package org.rex

import java.io.File

trait DistantLabeling[L] {

  type Label = L

  type Labels = Seq[Label]

  type Fn = Candidate => Option[Labels]
}

object DistLabelStr extends DistantLabeling[String] {

  type Query = String

  type Answer = String

  type KnowledgeBase = Map[Query, Map[Answer, Labels]]

  def apply(m: KnowledgeBase): Fn =
    (c: Candidate) =>
      for {
        amap <- m.get(normalize(c.queryW))
        labels <- amap.get(normalize(c.answerW))
      } yield {
        labels
      }

  /**
   * Removes: ', ", _ , (, ), [, ], {, }
   * and replaces all multi-whitespace with a single one
   */
  @inline def normalize(s: String) =
    s.trim.toLowerCase
      .replaceAll("'","")
      .replaceAll("\"","")
      .replaceAll("_","")
      .replaceAll(" +", " ")
      .replaceAll("\\(","").replaceAll("\\)","")
      .replaceAll("\\[","").replaceAll("\\]","")
      .replaceAll("\\{","").replaceAll("\\}","")
}

object Google50kRelationsDistLabelLoader {

  def apply(f: File):DistantLabeling[String]#Fn = {

    // we'll have to parse the JSON
    // get one triple (Q,A,Rel)
    // cross reference the ids (Q,A) with the names
    // normalize names

    ???
  }

}
