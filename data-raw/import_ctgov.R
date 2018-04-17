library(tidyverse)
library(stringr)
library(devtools)

path_to_file <- file.path('data-raw', 'raw_ctgov')
ct_files <- list.files(path_to_file)

imported_ct <- purrr::map(file.path(path_to_file, ct_files),
  ~ readr::read_delim(.,
      delim = '|',
      quote = '\"',
      na = c("", "NA", "N/A")
  )
)

imported_ct <- purrr:::map(imported_ct, ~ {
  names(.) <- tolower(names(.))
  .
})

names(imported_ct) <- ct_files

summaries_ct <- dplyr::full_join(
    imported_ct$brief_summaries.txt,
    imported_ct$detailed_descriptions.txt,
    by = c("id" = "id", "nct_id" = "nct_id")
  ) %>%
  dplyr::select(-id) %>%
  full_join(
    imported_ct$studies.txt %>%
      select(nct_id, first_received_date, brief_title, official_title),
    by = c('nct_id' = 'nct_id')
  ) %>%
  full_join(
    imported_ct$central_contacts.txt %>%
      select(nct_id, contact_type, name),
    by = c('nct_id' = 'nct_id')
  ) %>%
  transmute(
    final    = NA_integer_,
    id       = nct_id,
    title    = str_c(brief_title, official_title),
    year     = str_extract(first_received_date, '[0-9]{4}') %>% as.integer,
    authors  = str_c(name, ' (', contact_type, ')'),
    abstract = str_c(
      str_replace_na(description.x, replacement = ' '),
      str_replace_na(description.y, replacement = ' ')
    )
  )

devtools::use_data(summaries_ct, overwrite = TRUE, compress = 'xz')
