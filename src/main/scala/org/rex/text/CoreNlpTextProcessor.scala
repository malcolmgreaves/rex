package org.rex.text

import edu.arizona.sista.processors.Processor
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor

object CoreNlpTextProcessor {

  // implicit conversion from a Sista project Document type to a REx project Document
  import Document.sistaDoc2Doc

  /**
    * Uses the Processor to convert a block of text into a document.
    *
    * Conversion is based upon Sista + CoreNLP libraries. Uses the interfaces
    * defined here, however.
    *
    * The input ProcessorConf is assumed to be based upon what the Processor
    * uses. Caller is responsibile for verifying and agreeing to this assumption.
    */
  def apply(pConf: ProcessingConf, corenlpProcessor: Processor): TextProcessor =
    new TextProcessor {

      override val conf: ProcessingConf =
        pConf

      override def process(id: String, text: String): Document =
        (
          id, {
            val doc = corenlpProcessor.mkDocument(text)

            // here, doc is mutable
            // this is why we have these nested if statements w/o meaningful values
            // in their respective blocks

            if (conf.tagSet.isDefined) {
              corenlpProcessor.tagPartsOfSpeech(doc)

              if (conf.lemmatize) {
                corenlpProcessor.lemmatize(doc)

                if (conf.entSet.isDefined)
                  corenlpProcessor.recognizeNamedEntities(doc)

                if (conf.parse) {
                  corenlpProcessor.parse(doc)

                  if (conf.resolveCoreference)
                    corenlpProcessor.resolveCoreference(doc)

                  if (conf.discourse)
                    corenlpProcessor.discourse(doc)
                }
              }
            }

            doc
          }
        )
    }

  def apply(pConf: ProcessingConf): TextProcessor =
    apply(
      pConf,
      if (pConf.resolveCoreference)
        new CoreNLPProcessor(
          internStrings = false,
          basicDependencies = true,
          withDiscourse = pConf.discourse
        )
      else
        new FastNLPProcessor(
          internStrings = false,
          withDiscourse = pConf.discourse
        )
    )

}
