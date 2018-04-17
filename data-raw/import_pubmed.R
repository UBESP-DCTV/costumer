library(tidyverse)
library(stringr)
library(devtools)

path_to_file <- file.path('data-raw', 'raw_pubmed')
pubmed_files <- list.files(path_to_file)

summaries_pubmed <- purrr:::map(file.path(path_to_file, pubmed_files),
  ~ {
    data <- readxl::read_xlsx(.)
    names(data) <- tolower(names(data))
    mutate(data,
      final = as.integer(final),
      year  = as.integer(final)
    )
  }
)

names(summaries_pubmed) <- tolower(pubmed_files) %>%
  str_extract('^[^\\.]*')

devtools::use_data(summaries_pubmed, overwrite = TRUE, compress = 'xz')
