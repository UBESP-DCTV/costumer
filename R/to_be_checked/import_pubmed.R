#' Import RIS bibligraphic file
#'
#' This function is a sligthly modification of the original one provided by the
#' `ris` package by Stephanie Kovalchik <skoval@ucla.edu>, not available for the
#' present version of R (3.3.1). It only aims is to import a file RIS into a
#' list  R-structure.
#'
#' @param file <chr> the path of the RIS file
#' @return A list containing the imported entries
#' @export
read_ris <- function(file,...){

    ###DEPENDENT FUNCTIONS

    get.entry <- function(ty){
        #TAKE RIS TY CHARACTER/VECTOR TYPE AND RETURN BIBTEX TYPE
        #IF MULTIPLE ENTRIES TAKE THE ONE THAT FOLLOWS RIS FORM

        if(length(ty)>1){
            ty <- ty[grep("[A-Z]{2,}",ty)]
        }

        if(ty%in%c("ABST","INPR","JFULL","JOUR","MGZN","NEWS")){
            return("article")
        }
        else if(ty%in%c("SER","BOOK")){
            return("book")
        }
        else if(ty%in%c("CONF","CHAP")){
            return("incollection")
        }
        else if(ty=="THES"){
            return("phdthesis")
        }
        else{
            #RPRT, UNPB AS MISC
            return("misc")
        }
    }

    make.reference <- function(lines){

        get.date.info <- function(date){
            #PROCESS RIS CHARACTER DATE INTO LIST WITH YEAR,MONTH,DAY,INFO
            if(length(grep("/",date)!=0)){
                i <- grep("/",date)
                date.list <- as.list(strsplit(date[i],"/")[[1]])
                if(length(date.list)==3){
                    names(date.list) <- c("year","month","day")
                    date.list$info <- ""
                }
                else{
                    names(date.list) <- c("year","month","day","info")
                }
            }
            else{
                #TAKE YEAR IF DOES NOT FOLLOW RIS CONVENTION
                date <- sub(".*([0-9]{4,4}).*","\\1",date[1])
                date.list <- list(year=date,month="",day="",info="")
            }
            return(date.list)
        }

        get.content <- function(index){
            #GENERAL FUNCTION TO EXTRACT CONTENT OF A GIVEN FIELD LINE
            content <- strsplit(lines[index],"[A-Z]([A-Z]|[0-9])  - ")
            content <- unlist(content)
            content[content!=""]
        }

        field.names <-  c(
            "ris.type",
            "type",
            "id",
            "title.primary",
            "title.secondary",
            "author.primary",
            "author.secondary",
            "keyword",
            "date",
            "abstract",
            "journal",
            "journal.abbrev",
            "volume",
            "number",
            "city",
            "publisher",
            "isbn",
            "address",
            "pages",
            "url"
        )

        #RIS CORRESPONDING FIELD PATTERNS
        ris.type <- "^TY"
        ris.id <- "^ID"
        ris.title.primary <- "(^T[1I])|(^BT)|(^CT)"
        ris.title.secondary <- "^T[23]"
        ris.author.primary <- "^A[1U]"
        ris.author.secondary <- "(^A[23])|(^ED)"
        ris.keyword <- "^KW"
        ris.date <- "(^Y1)|(^PY)"
        ris.abstract <- "(^N1)|(^N2)|(^AB)"
        ris.journal <- "^J[FO]"
        ris.journal.abbrev <- "JA"
        ris.volume <- "^VL"
        ris.number <- "(^IP)|(^CP)"
        ris.city <- "CY"
        ris.publisher <- "PB"
        ris.isbn <- "^SN"
        ris.address <- "^AD"
        ris.pages <- "(^[ES]P)|(^PG)"
        ris.url <- "^UR"

        fields <- c(
            ris.type,
            ris.type,
            ris.id,
            ris.title.primary,
            ris.title.secondary,
            ris.author.primary,
            ris.author.secondary,
            ris.keyword,
            ris.date,
            ris.abstract,
            ris.journal,
            ris.journal.abbrev,
            ris.volume,
            ris.number,
            ris.city,
            ris.publisher,
            ris.isbn,
            ris.address,
            ris.pages,
            ris.url
        )

        field.index <- sapply(fields,function(pattern){grep(pattern,lines)})

        i <- sapply(field.index,function(x){length(x)!=0})

        if(any(i)){
            ref <- lapply(field.index[i],get.content)
            names(ref) <- field.names[i]
            ref[2] <- get.entry(ref[2])

            if("date"%in%names(ref)){
                ref$date <- get.date.info(ref$date)
                ref$year <- as.numeric(ref$date$year)
            }
            else{
                ref$year <- NA
            }
        }
        else{
            ref <- list()
        }
        return(ref)
    }

    ###
    lines <- readLines(con=file,...)

    #REMOVE SPACE
    lines <- lines[lines!=""]

    #INDEX OF BEGINNING AND END OF EACH CITATION
    stops <- grep("^ER",lines)-1

    starts <- c(1,stops[-length(stops)]+1)

    citations <- lapply(1:length(starts),function(index){
        ref.list <- make.reference(lines[starts[index]:stops[index]])
        return(ref.list)
    })

    return(citations)
}




#' Collapse sub-list into list
#'
#' This funciton take a list and collapse each his elements which is a list
#' into a simple character vector (standard separator is '-').
#'
#' @param field <list> a list
#'
#' @return A list for which each element is a character vector
#'
collapse_field <- function(field) purrr::map(field, `[[`, 1)





#' Import PUBMED after RIS conversion
#'
#' The aim of this function is to take an exported/transformed ris file from
#' pubmed and import it into R.
#'
#' @param ris <chr> the path of the \code{.ris} file containing the exported
#'                  results from a pubmed query
#' @return A data frame containing the exported records as row and the
#'         columns corresponding to the fields provided by pubmed

#' @importFrom plyr rbind.fill
#' @export
import_pubmed <- function(ris) {

    read_ris(ris) %>%
        purrr::map(mlnursect:::collapse_field) %>%
        purrr::map(~plyr::rbind.fill(dplyr::as_data_frame(.))) %>%
        plyr::rbind.fill() %>%
        dplyr::as_data_frame() %>%
        dplyr::select(-date) %>%
        dplyr::mutate(
            ris.type = as.factor(ris.type),
            type     = as.factor(type),
            id       = as.integer(id),
            year     = as.integer(year)
        )
}
