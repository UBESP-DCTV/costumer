# costumer 0.0.6

* Added dependencies from `mlr` package needed for `unbalanced` ones
* Added `inst/analyses/hutch_analyses_p1_v2.0.R` file which is the script 
    used to perform the final analyses.


# costumer 0.0.5

* Added `data-raw/raw_pubmed/` folder with the training data.
* Added `data-raw/import_pubmed.R` to import pubmed trainig data.
* Added `non_git_nor_build_derived_data/` folder for (big) derived data
    which has to be stored but do not go under VCS nor into the package
* Added `data-raw/raw_ctgov/` folder with the snapshot taken for test.
* Added `inst/doc/AACT201603_comprehensive_data_dictionary.xlsx` as a data
    dictionary for the original clinicaltrial.gov data.
* Added `import_ctgov.R` and `ct_corpus_and_dtm.R` to import test data and
    create the relative corpus and dtm.
* Added `caret_methods_cvAble.R` script to generate `data/*.cvAble.rda`s
    (with * in {glmnet, knn, LogitBoost, nb, rf, svmLinear2}) to update the
    internal `caret` corresponding functions in a way they can reweight iDF
    weights internally to the crossvalidation step.
* Added `data_RUSROS_new.R` script to generate `data/*_new.rda`s
    (with * in {ROS3565, ROS5050, RUS3565, RUS5050}) to add to the `caret`
    plateau of resampling strategies the ones of our interest.
* Updated data documentation.


# costumer 0.0.4

* Added imports.
* Added functions.
* Added tests (all passed).


# costumer 0.0.3

* Updated `README.Rmd` removing install instructions from CRAN



# costumer 0.0.2

* Added `data-raw` folder for raw data.
* Added support for osx on Travis-CI



# costumer 0.0.1

* Added support for Travis-CI for integration within unix machine.
* Added support for AppVeyor for integration within Windows machines.
* Added support for codecov for testing coverage.



# costumer 0.0.0.9000

* Added `testthat` support for automated testing.
* Added a `.gitignore` file to configure git ignored files.
* Added a `.Rbuildignore` file to configure pkg build ignored files.
* Set the License field in DESCRIPTION to 'GPL-3'.
* Added a `LICENSE.md` file reporting the "GNU General Public License, v.3".
* Added a `README.Rmd` file to produce the README in Rmarkdown.
* Added a `NEWS.md` file to track changes to the package.
