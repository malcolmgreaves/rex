package org.rex.dl

import org.rex.relation_extract.Candidate

trait DistantLabeling[L] {

  type Label = L

  type Labels = Set[Label]

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

  def relationsOf(kb: KnowledgeBase): Set[Label] =
    kb.foldLeft(Set.empty[Label]) {
      case (labelSet, (_, amap)) =>
        amap.foldLeft(labelSet) {
          case (ls, (_, relations)) =>
            ls ++ relations
        }
    }

  /**
    * Removes: ', ", _ , (, ), [, ], {, }
    * and replaces all multi-whitespace with a single one
    */
  @inline def normalize(s: String) =
    s.trim.toLowerCase
      .replaceAll("'", "")
      .replaceAll("\"", "")
      .replaceAll("_", "")
      .replaceAll(" +", " ")
      .replaceAll("\\(", "")
      .replaceAll("\\)", "")
      .replaceAll("\\[", "")
      .replaceAll("\\]", "")
      .replaceAll("\\{", "")
      .replaceAll("\\}", "")
}
