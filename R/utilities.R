#' This a reference table of NHIC data items. 
#'
#' @name data.checklist
#' @docType data
#' @author Sinan Shi \email{s.shi@ucl.ac.uk}
#' @keywords data
NULL




#' get indexing tables for time label, time-wise value, meta data label, and
#' meta data.  
#' @return list of vectors contains time.index, datat.index, meta.index,
#'          datam.index
extractIndexTable <- function() {
    info <- extractInfo()

    checklist <- list()
    for(i in seq(info$nontime))
        checklist[[info$nontime[i]]] <- "item1d"
    for(i in seq(info$time$idt))
        checklist[[info$time$idt[i]]] <-"time"
    for(i in seq(info$time$id))
        checklist[[info$time$id[i]]] <- "val"
    for(i in seq(info$meta$meta))
        checklist[[info$meta$meta[i]]] <- "meta"
    
    return(checklist)
}


# This is a simplified version of as.data.frame() with better performance. 
.simple.data.frame <- function(x) {
    nm <- names(x)
    attr(x, "row.names") <- .set_row_names(length(x[[1]]))
    attr(x, "col.names") <- nm
    class(x) <- "data.frame"
    x
}



.which.type <- function(id) {
    return(checklist[[id]])
}


.which.datatype <- function(id) {
  # List with the conversion operations to do for each data type
  operations <- list('numeric' = as.numeric,
                     'text' = as.character,
                     'date' = as.character, # They are hashed for now
                     'time' = as.character, # They are hashed for now
                     'logical' = as.logical,
                     'list' = as.character,
                     'date/time' = as.character, # They are hashed for now
                     'list / logical' = as.character) # what are they?

  datatype = ITEM_REF[[id]]$Datatype
  if (!is.null(datatype)){
    if (exists(datatype, operations)){
      return(operations[[datatype]])
    }
  }
  # accounts for not listed or null (eg. when working with dt labels)
  return(as.character)
}

#' give id number from NHIC code like "NIHR_HIC_ICU_xxxx"
#' @param nhic NHIC code
whichIsCode <- function(nhic) {
    return(grepl(nhic, pattern="[0-9][0-9][0-9][0-9]"))
}

#' extract information from data.checklist
#' @return list of time [data.frame(id, idt)], meta [data.frame(id, idmeta)], 
#'         nontime [numeric], MAX_NUM_NHIC
#' @export extractInfo
extractInfo <- function() {
    index.time <- whichIsCode(data.checklist$NHICdtCode) 
    index.meta <- whichIsCode(data.checklist$NHICmetaCode)

    item.labels <- StdId(data.checklist$NHICcode[index.time])
    time.labels <- StdId(data.checklist$NHICdtCode[index.time])

    metaitem.labels <- StdId(data.checklist$NHICcode[index.meta])
    meta.labels <- StdId(data.checklist$NHICmetaCode[index.meta])
    
    time.list <-
        data.frame(id=item.labels@ids, idt=time.labels@ids,
                   stringsAsFactors=FALSE)
    meta.list <- data.frame(id=metaitem.labels@ids, meta=meta.labels@ids,
                            stringsAsFactors=FALSE)
    
    
    nontime<- StdId(data.checklist$NHICcode[!index.time])
    # get all ids which should be the assemble of NHICcode and NHICmetaCode
    all.nhic.code <- StdId(data.checklist$NHICcode)
    all.ids <- c(meta.list$idmeta,
                 all.nhic.code@ids)
    if (any(duplicated(all.ids)))
        stop("data.checklist.RData error! meta data code and NHICcode are overlaped")
    return(list(time=time.list, meta=meta.list, nontime=nontime@ids,
                MAX_NUM_NHIC=max(as.numeric(as.number(all.nhic.code)), 
                                 as.numeric(as.number(StdId(time.list$idt))))))
}

#' Lookup items information by keywords
#' 
#' This function tries to match keywords in short names, long names and NHIC code. 
#' The matched items will be displayed. 
#' @param keyword character e.g. "h_rate", "heart", "108".
#' @param style character, the style of the table output which can be "simple",
#' "rmarkdown", and "grid"
#' @return character the short names of the selected items.
#' @export lookup.items
lookup.items <- function(keyword, style="grid") {
    
    index1 <- grep(keyword, stname2longname.dict, ignore.case=T)
    index2 <- grep(keyword, names(stname2longname.dict), ignore.case=T)
    index3 <- grep(keyword, stname2code(names(stname2longname.dict)), ignore.case=T)


    stn <- unique(names(stname2longname.dict[c(index1, index2, index3)]))
    query_item_ref <- function(stn, field)
        unlist(sapply(ITEM_REF[stname2code(stn)], 
                      function(x) {
                          ifelse(is.null(x[[field]]), "N/A", x[[field]])
                      }))

    query_item_levels <- function(stn) {
        txtv <- vector()
        for(s in stn) {
            i <- ITEM_REF[[stname2code(s)]][['levels']]
            if (is.null(i))
                txtv <- c(txtv, "N/A")
            else {
                nms <- names(i)
                txt <- ""
                for(j in seq(i)) 
                    txt <- paste0(txt, "[", nms[j],"]: ", i[j], "\n")
                txtv <- c(txtv, txt)
            }
        }
        return(txtv)
    }

    tb <- data.frame("NHIC Code"=stname2code(stn), 
               "Short Name"=stn, 
               "Long Name"=stname2longname(stn), 
               "Unit"=query_item_ref(stn, "Units"), 
               "Data type"=query_item_ref(stn, "Datatype"),
               "Levels"=query_item_levels(stn))
    rownames(tb) <- NULL
    pander(tb, style=style, split.table = Inf)
    invisible(tb[, 2])
}


#' Convert time from xml to POSIX format.
#'
#' Convert the XML time The XML time format to POSIXct. 
#' @param xml.time character. Time in XML format such as 2014-02-01T03:00:00
#' @param allow logical. Wrong format will be accepted when \code{allow} is set
#' to be TRUE and NA will be the return value, otherwise return error. 
#' It is useful while dealing with pseudonymous data where the time format is
#' not presented correctly. 
#' 
#' @export
xmlTime2POSIX <- function(xml.time, allow=FALSE){
    if (is.null(xml.time))
        return(NA)
    tp <- as.POSIXct(xml.time, "GMT", format="%Y-%m-%dT%H:%M:%S")
    tp[is.na(tp)] <- as.POSIXct(xml.time[is.na(tp)], "GMT", format="%Y-%m-%d")
    if (!allow) {
        if(any(is.na(tp)))
            stop(xml.time[is.na(tp)])
    }
    return(tp)
}

#' Produce a site id reference table.
#'
#' @return data.frame 
#' @export site.info
site.info <- function(){
    si <- list(
               "D20N"=c("Addenbrooke's Hospital", "Neuro", "Cambridge", ""),  
               "D20"=c("Addenbrooke's Hospital", "ICU/HDU", "Cambridge", "John Farnham"), 
               "K32"=c("Guy's Hospital", "ICU", "GSTT", ""),   
               "V47"=c("St Thomas' Hospital", "ICU/HDU", "GSTT", ""),   
               "H09"=c("St Thomas' Hospital", "OIR", "GSTT", "overnight intensive recovery"), 
               "Z89"=c("St Thomas' Hosptial", "HDU", "GSTT", "page and victoria"), 
               "C90"=c("Hammersmith Hospital", "ICU/HDU", "Imperial", ""),  
               "Q13"=c("St Mary's Hospital London", "ICU", "Imperial", "Milne Ward"),
               "S28"=c("John Radcliffe", "ICU", "Oxford", ""),
               "F42"=c("Oxford Neuro", "", "Oxford", "no data"),
               "F54"=c("Oxford Horton", "", "Oxford", "no data because not on ICIP"),
               "Q70"=c("University College Hospital", "ICU/HDU", "UCLH", ""),
               "Q70W"=c("Westmoreland Street", "ICU/HDU", "UCLH", ""),   
               "R42"=c("Unknown", "Unknown", "Unknown", "Unknown"),
               "X90"=c("Addenbrooke's Hospital", "General/Liver/Transplant",  "Cambridge", "John Farnham")
               )
    si <- data.frame(t(.simple.data.frame(si)), stringsAsFactors=F)
    names(si) <- c("Hospital", "Unit", "Trust", "Comments")
    return(si)
}



#' ICNARC diagnosis reference table 
#'
#' @name icnarc
#' @references \url{https://www.icnarc.org/Our-Audit/Audits/Cmp/Resources/Icm-Icnarc-Coding-Method}
#' @docType data
#' @keywords data
NULL


#' Convert ICNARC codes to diagnosis (text)
#' 
#' NOTE: There are still ~600 code missing. see issue #133
#' @param icnarc the ICNARC code, e.g. 1.1.1.1.1
#' @param levels category level, from [1 - 5]. TODO level 4. 
#' @param surgery T/F with or without surgical information 
#' @return character ICNARC diagnosis
#' @export 
icnarc2diagnosis <- function(icnarc, surgery=TRUE, levels=NULL) {
    if (is.null(icnarc)) return("NA")
    # e.g 1.01.1 -> 1.1.1
    if(!is.null(levels))
        icnarc <- icnarc.breakdown(icnarc, digits=levels)
    else 
        icnarc <- sapply(lapply(strsplit(icnarc, split='[.]'), as.numeric), 
          function(x) paste(x, collapse="."))

    diag <- as.character(icnarc.dict[icnarc])
    if (!surgery)
        return(gsub(x=diag, " [(]Surgical[)]| [(]Nonsurgical[)]", ""))
    else
        return(diag)
}

icnarc.breakdown <- function(r, digits=3) {
    cols <- strsplit(r, '[.]')
    cols <- lapply(cols, 
           function(x) {
               x <- tryCatch(as.numeric(x), 
                             warning=function(w) {
                                 return(NA)
                             })
               if (length(x) < 5)
                   x <- c(x, rep(NA, 5-length(x)))
               if (length(x) > 5)
                   x <- x[1:5]
               x
           })
    
    cols <- t(data.frame(cols))
    rownames(cols) <- seq(nrow(cols))
    cols <- cols[, 1:digits]

    combine <- function(x) {
        x <- x[!is.na(x)]
        paste(x, collapse=".")
    }
    if (digits == 1) return(as.character(cols))
    if (class(cols) == "numeric") 
        return(combine(cols))
    else
        return(apply(cols, 1, combine))
}
