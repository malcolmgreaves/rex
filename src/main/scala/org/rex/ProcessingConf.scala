package org.rex

/**
 * Information about the configuration of a natural language parser.
 *
 * @param entSet Information about the named entities that the associated processor uses, if applicable.
 * @param tagSet Information about the part-of-speech tags that the associated processor uses, if applicable.
 * @param parse Whether or not the associated parser should construct a syntactic parse tree of sentences.
 * @param lemmatize Whether or not the associated parser should perform lemmatization. Needs POS tagging.
 * @param resolveCoreference Whether or not the associated parser should perform co-reference resolution. Needs parsing.
 * @param discourse Whether or not the associated parser should perform additional discourse parsing. Needs parsing.
 */
case class ProcessingConf(
  entSet: Option[NeTagSet],
  tagSet: Option[PosTagSet],
  parse: Boolean,
  lemmatize: Boolean,
  resolveCoreference: Boolean,
  discourse: Boolean)

object ProcessingConf {

  object DefaultProcConf {

    implicit val procConf = ProcessingConf(
      entSet = Some(NeTagSet.Default4Class.entSet),
      tagSet = Some(PosTagSet.DefaultPennTreebank.posSet),
      parse = true,
      lemmatize = true,
      resolveCoreference = true,
      discourse = false
    )
  }

}

/**
 * Contains information about a set of named entity (NE) tags.
 *
 * @param tags All valid named entity tags. Does not include the nonEntityTag.
 * @param nonEntityTag A value that represents a non-existant tag.
 */
case class NeTagSet(tags: Set[String], nonEntityTag: String)

object NeTagSet {

  /** Contains implicit NamedEntitySet with PERSON, LOCATION, and ORGANIZATION tags and "" non-entity tag */
  object Default4Class {

    implicit val entSet = NeTagSet(
      tags = Set("PERSON", "LOCATION", "ORGANIZATION", "DATE"),
      nonEntityTag = "O"
    )
  }

}

/**
 * Contains information about a set of part of speech (POS) tags.
 *
 * @param punctuation Subset of `tags`, parts of speech that are punctuation.
 * @param nouns Subset of `tags`, parts of speech that are nouns.
 * @param verbs Subset of `tags`, parts of speech that are verbs.
 * @param misc Subset of `tags`, a catch-all for parts of speech that do not fit into one of the predefined tag subsets.
 */
case class PosTagSet(
    punctuation: Set[String],
    adjectives: Set[String],
    nouns: Set[String],
    pronouns: Set[String],
    verbs: Set[String],
    adverbs: Set[String],
    misc: Set[String]) {

  /**
   * All part-of-speech tags.
   *
   * Equivalent to:
   * `punctuation ++ adjectives ++ nouns ++ pronouns ++ verbs ++ adverbs ++ misc`
   */
  final val tags: Set[String] =
    punctuation ++ adjectives ++ nouns ++ pronouns ++ verbs ++ adverbs ++ misc
}

object PosTagSet {

  object DefaultPennTreebank {

    implicit val posSet = PosTagSet(
      punctuation = Set("$", "``", "''", "(", ")", ",", "--", ".", ":", "SYM"),
      adjectives = Set("JJ", "JJR", "JJS"),
      nouns = Set("NN", "NNP", "NNPS", "NNS"),
      pronouns = Set("PRP", "PRP$"),
      verbs = Set("VB", "VBD", "VBG", "VBN", "VBP", "VBZ"),
      adverbs = Set("RB", "RBR", "RBS"),
      misc = Set(
        "CC",
        "CD",
        "DT",
        "EX",
        "FW",
        "IN",
        "LS",
        "MD",
        "PDT",
        "POS",
        "RP",
        "TO",
        "UH",
        "WDT",
        "WP",
        "WP$",
        "WRB"
      )
    )
  }

}