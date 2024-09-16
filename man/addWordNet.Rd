% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wordnet.R
\name{addWordNet}
\alias{addWordNet}
\title{Use WordNet to assist concept database creation}
\usage{
addWordNet(CDB_TABLE, wn_categories, WN, CHECK_TABLE = NULL)
}
\arguments{
\item{CDB_TABLE}{data.frame or data.table with columns
conceptId (integer64) and term (character, with space before
and after) containing existing descriptions in the CDB}

\item{wn_categories}{WordNet categories to use}

\item{WN}{WordNet data.table as returned by downloadWordnet}

\item{CHECK_TABLE}{other table in the same format as CDB_TABLE
to check for WordNet synonyms that link to another unrelated
concept, where this synonym will be excluded because of the risk
of errors}
}
\value{
CDB_TABLE with extra rows for Wordnet synonyms
}
\description{
Adds terms from a WordNet thesaurus to a concept database, matching
on term. It is recommended to restrict the wordnet categories to
ensure that words with multiple meanings are not linked to the wrong
synonym.
}
\examples{
WORDNET <- data.table::data.table(cat = c('noun.body', 'noun.state'),
  wordnetId = bit64::as.integer64('1', '2'),
  synonyms = list(c('heart', 'pump', 'ticker'),
  c('infection', 'infectious')),
  parents = list('cardiovascular system',
  'pathologic process'), 
  adj = list('cardiac', 'infectious'))
# Add Wordnet synonyms to a concept database table
SNOMED <- sampleSNOMED()
CDB_TABLE <- description(c('Heart', 'Infection'),
  include_synonyms = TRUE)[type == 'Synonym',
  .(conceptId, term = paste0(' ', tolower(term), ' '))]
addWordNet(CDB_TABLE, 'noun.state', WORDNET)
}
\references{
\url{https://wordnet.princeton.edu/}
}
\seealso{
[downloadWordNet()]
}