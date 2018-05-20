library(readxl)
library(devtools)
library(costumer)

#> Data training related to the Liu metanalysis (his 23 positive sample and
#> 400 randomly sampled from pubmed on the complementary of its searchstring).
#>
#> Downloaded from PUBMED at 05-april-2017.
#> Data retrieved were inserted in an xlsx file including the classification
#> (i.e. 1/0 --> pertinent/non-pertinent), ID, Title, Year, Author and Abstract.
#
liu_4h28 <- readxl::read_xlsx(
  file.path('data-raw', 'random4h28.xlsx')
)

liu_4h28[['final']] <- as.integer(liu_4h28[['final']])
liu_4h28[['year']]  <- as.integer(liu_4h28[['year']])

names(liu_4h28) <- tolower(names(liu_4h28))

devtools::use_data(liu_4h28, compress = 'gzip')

#> create the corpus and the dtm
liu_corpus <- create_train(liu_4h28, 'Liu')
liu_dtm    <- atom_dtm(liu_corpus)

devtools::use_data(liu_corpus)
devtools::use_data(liu_dtm)
