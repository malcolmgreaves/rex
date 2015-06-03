package org.rex

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
        amap <- m.get(c.queryW)
        labels <- amap.get(c.answerW)
      } yield {
        labels
      }
}
