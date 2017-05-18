#' S4 class to hold standard IDs such as "NIHR_HIC_ICU_0001"
#' @slot ids single or multiple characters
#' @import methods
StdId <- setClass ("StdId",
                   slots = c(ids="vector"), 
                   validity=function(object)
                   {
                       reg <- regexpr("NIHR_HIC_ICU_[0-9]+", object@ids)
                       ids <- regmatches(object@ids, reg)
                       if (length(object@ids) != length(ids)) 
                           return("initialisation failure, as the standard ID pattern cannot be found.")
                       else
                           object@ids = ids
                       return(TRUE)
                   })


#' constructor of StdId class
#'
#' @param text NIHC code which should be in a format like NIHR_HIC_ICU_xxxx
#' @export StdId
StdId <- function(text) {
    reg <- regexpr("NIHR_HIC_ICU_[0-9]+", text)
    ids <- regmatches(text, reg)
    if (length(text) != length(ids) | length(text) == 0) 
        stop("StdId: initialisation failure, as the standard ID pattern cannot be found.")
    else
        return(methods::new("StdId", ids=ids))
}


#' Convert standard IDs to numbers (character) which can be used for indexing.
#'
#' @param obj a StdId object. 
#' @export as.number
as.number <- function(obj) {
    if(!all(grepl("NIHR_HIC_ICU_", obj@ids)))# input is code alredy
        return(obj@ids)
    no.prefix <- 
        unlist(strsplit(obj@ids, "NIHR_HIC_ICU_"))[seq(2, length(obj@ids) * 2, 2)]
    return(as.character(no.prefix))
}

.as.number <- function(code) {
    return(as.numeric(strsplit(code, "NIHR_HIC_ICU_")[[1]][2]))
}


all.nhic.code <- function(cls) {
        data.checklist[data.checklist$Classification1 == cls,"NHICcode"]
}

#' Convert NHIC codes to the short names
#'
#' @param code character NIHC code, e.g. NIHR_HIC_ICU_0108
#' @return shortname character e.g. h_rate 
#' @export code2stname
code2stname <- function(code) {
    code <- as.character(code)
    stn <- code2stname.dict[code]
    stn[is.na(stn)] <- code[is.na(stn)]
    return(stn)
}

#' Convert short names to NHIC codes
#' 
#' @param stname character short names of data item h_rate 
#' @return NIHC code character such as NIHR_HIC_ICU_0108
#' @export stname2code
stname2code <- function(stname) {
    stname <- as.character(stname)
    code <- stname2code.dict[stname]
    code[is.na(code)] <- stname[is.na(code)]
    return(code)
}

#' Convert short names to long names. 
#' 
#' @param stname character short names of data item h_rate 
#' @return longname character such as "heart rate"
#' @export 
stname2longname <- function(stname) {
    stname <- as.character(stname)
    code <- stname2longname.dict[stname]
    code[is.na(code)] <- stname[is.na(code)]
    return(code)
}


#' Identify the classification - classification1 
#' 
#' Identify the classification of a given item code or short
#' name. Classification1 has 5 labels: 
#' [1] "Demographic", [2] "Physiology" 
#' [3] "Drugs" [4] "Nursing_other" [5] "Laboratory"
#' @param item_name NHIC code or the short name
#' @return character the item classification
#' @export which.classification
which.classification <- function(item_name) {
    cls <- class.dict_code[item_name]
    if (is.na(cls))
        cls <- class.dict_stname[item_name]
    if (is.na(cls)) stop(paste("item name", item_name, "cannot be found."))
    return(cls)
}


#' Check if the item NHIC code or short name belongs to the demographic
#' category.
#' @param item_name character the NHIC code or the short name
#' @return logical
#' @export is.demographics
is.demographics<- function(item_name) {
    return(which.classification(item_name) == "Demographic")
}

#' Check if the items NHIC code or short names belong is longitudinal.
#' @export 
is.longitudinal <- function(item_name) {
    long <- !sapply(item_name, is.demographics)
    names(long) <- item_name
    return(long)
}




#' Check if the item NHIC code or short name belongs to the physiology
#' category.
#' @param item_name character the NHIC code or the short name
#' @return logical
#' @export is.physiology
is.physiology <- function(item_name) {
    return(which.classification(item_name) == "Physiology")
}


#' Check if the item NHIC code or short name belongs to the drugs 
#' category.
#' @param item_name character the NHIC code or the short name
#' @return logical
#' @export is.drugs
is.drugs <- function(item_name) {
    return(which.classification(item_name) == "Drugs")
}


#' Check if the item NHIC code or short name belongs to the Laboratory 
#' category.
#' @param item_name character the NHIC code or the short name
#' @return logical
#' @export is.laboratory
is.laboratory <- function(item_name) {
    return(which.classification(item_name) == "Laboratory")
}
