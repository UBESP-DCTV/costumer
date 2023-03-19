source("renv/activate.R")

if (interactive()) {
  library(targets)
  library(tarchetypes)
}

run_pipeline <- function(
    first_target_only = FALSE,
    first_outdated = FALSE,
    proceed = TRUE,
    save_all = TRUE,
    future = FALSE,
    workers = 2
) {
  if (interactive()) {
    if (
      requireNamespace("rstudioapi") &&
      rstudioapi::isAvailable() &&
      save_all
    ) {
      rstudioapi::documentSaveAll()
    }

    targets::tar_visnetwork(
      targets_only = first_target_only,
      outdated = first_outdated
    ) |>
      print()

    proceed <- usethis::ui_yeah(
      "Proceed with {usethis::ui_code('tar_make')}?"
    )
  }

  if (proceed) {
    withr::with_envvar(
      list(RSTUDIO_VERSION = "2021.09.0"), {
        if (future) {
          targets::tar_make_future(workers = 2)
        } else {
          targets::tar_make()
        }
      }
    )
    targets::tar_visnetwork(targets_only = TRUE) |>
      print()
  }
}
