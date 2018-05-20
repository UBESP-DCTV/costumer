#' Import XML output from a WHO export
#'
#' @param xml <chr> the path of the \code{.xml} file exported from the WHO
#'                  search website
#'                   (\url{http://apps.who.int/trialsearch/advsearch.aspx})
#' @return A data frame containing the exported records as row and all the 36
#'         columns provided by the WHO website.
#' @importFrom magrittr %>%
#' @export

import_who <- function(xml) {

    XML::xmlToDataFrame(xml, stringsAsFactors = FALSE) %>%
        dplyr::as_data_frame() %>%
        dplyr::mutate(
            Last_Refreshed_on  = lubridate::dmy(Last_Refreshed_on),
            Date_registration  = lubridate::dmy(Date_registration),
            Export_date        = lubridate::dmy_hms(Export_date),
            Source_Register    = as.factor(Source_Register),
            Recruitment_Status = as.factor(Recruitment_Status),
            other_records      = as.factor(other_records)
        )
}


#' Import XML single trial from a WHO export
#'
#' @param xml <chr> the path of the \code{.xml} file exported from the WHO
#'                  search website
#'                   (\url{http://apps.who.int/trialsearch/advsearch.aspx})
#' @return A data frame containing the exported records as row and all the 36
#'         columns provided by the WHO website.
#' @importFrom magrittr %>%
#' @export
import_singles_who <- function(xml) {

        doc <- XML::xmlParse(xml)
        res <- XML::getNodeSet(
            doc       = doc,
            path      = c(
                "/who:DataSet/diffgr:diffgram/NewDataSet/Trial",
                "/who:DataSet/diffgr:diffgram/NewDataSet/Criteria",
                "/who:DataSet/diffgr:diffgram/NewDataSet/Health_condition",
                "/who:DataSet/diffgr:diffgram/NewDataSet/Intervention",
                "/who:DataSet/diffgr:diffgram/NewDataSet/Primary_outcome",
                "/who:DataSet/diffgr:diffgram/NewDataSet/Secondary_outcome",
                "/who:DataSet/diffgr:diffgram/NewDataSet/Secondary_IDs"
            ),
            namespace = c(
                who = "http://www.who.int",
                diffgr="urn:schemas-microsoft-com:xml-diffgram-v1"
            )
        ) %>%
        map(~XML::getChildrenStrings(.)) %>%
        unlist %>%
        t %>%
        as_data_frame

        if(sum(colnames(res) == 'Outcome_Name') > 0) {

            res$Outcome <- map_chr(res[colnames(res) == 'Outcome_Name'], ~.) %>%
                        str_c(collapse = ' - ')

            res[colnames(res) == 'Outcome_Name'] <- NULL
        }
        res
}
