#' Process the EHR data in table format
#'
#' ccRecord data are re-arranged into tables where the columns stands for 
#' data fields (e.g. heart rate, blood pressure) and the rows stands for 
#' each data record within a unique cadence. See ccTable_create_cctable.
#' ccTable is the data processing platform. It stores both original data 
#' and processed data alongside with the process details. It also contains 
#' various commonly used data filters. 
#' @field record ccRecord.
#' @field conf the YAML style configuration.
#' @field torigin the original data table.
#' @field tclean the data table after cleaning processes. 
#' @field dfilter list contains data filtering information.
#' @field dquality list contains data quality.
#' @field summary list
#' @field base_cadence the base cadence is specified in hours
#' @include ccRecord.R
#' @examples
#' rec <- ccRecord()
#' cctable <- create_cctable(rec, freq=1)
#' cctable <- cctable$clean()
#' #table <- cctable$tclean 
#' @exportClass ccTable
ccTable <- setRefClass("ccTable", 
                       fields=c(
                                record="ccRecord", 
                                conf="list",
                                torigin="data.table", 
                                tclean="data.table",
                                dfilter="list",
                                dquality="list",
                                summary="list",
                                base_cadence="numeric",
                                .rindex="data.table", 
                                .epindex="data.table",
                                items="character"))
ccTable$methods(
                show = function() {
                    cat("$tclean", "\n")
                    #print(.self$tclean)
                    cat("Data entry (origin) = ", nrow(.self$torigin), "\n")
                    uniepisode <- .self$torigin[,1,by=c("episode_id", "site")]
                    cat("Episode number (origin) = ", nrow(uniepisode), "\n")
                    cat("The base cadence is ", .self$base_cadence, " hour.\n")
                })

#' Create a ccTable object
#'
#' Re-arrange the ccRecord object to table format where each column stands 
#' for a variable and each row a record data point. The number of rows will 
#' depend on the sampling frequency set in this function. If the original data
#' has a higher recording frequency than the set frequency (freq), the closest 
#' data point will be taken. It is suggested the `freq` should not be set 
#' lower than the maximum recording frequency in the original dataset. 
#' @param rec ccRecord
#' @param conf either the path of YAML configuration file or the configuration
#' @param freq a unique sampling frequency (in hours) for all variables. e.g. if freq is set to 
#' 1, each row in ccTable will represent a record of one hour. 
#' @return ccTable
#' @export
create_cctable <- function(rec, conf=NULL, freq=1) {
    if (is.null(conf)) 
        conf <- ITEM_REF
    else { 
        if (is.character(conf))
            conf <- yaml.load_file(conf)
    }

    cct <- ccTable(record=rec, conf=conf)
    cct$create_table(freq)
    return(cct)
}

#' Create a ccTable object
#'
#' This is a member function of ccTable-class. Using create_cctable is a safer and 
#' easier way to create the ccTable. See create_cctable. 
#' @name ccTable_create_cctable
NULL
ccTable$methods(
                create_table = function(freq){
                    "Create a table contains the selected items in the conf with a given
                    frequency (in hour)"
                    .self$items <- names(.self$conf)
                    .self$torigin <- ccd_select_table(record=record, items_opt=items, freq=freq)
                    if (nrow(.self$torigin) != 0) {
                        setkey(.self$torigin, "site", "episode_id")
                        .self$tclean <- .self$torigin
                        setkey(.self$torigin, "site", "episode_id")
                        .self$base_cadence <- freq

                        .self$.rindex <- .self$torigin
                        for(i in .self$items) .self$.rindex[[i]] <- TRUE

                        .self$.epindex <- .self$torigin[, TRUE, by=c("site", "episode_id")]
                        setnames(.self$.epindex, c("site", "episode_id", "index"))
                    } else 
                        .self$torigin <- data.table(site=character(), 
                                                    episode_id=character(),
                                                    time=integer())
                })

ccTable$methods(
                update.entry = function(){
                    for (i in .self$items) 
                        .self$tclean[[i]][!.self$.rindex[[i]]] <- NA
                })

ccTable$methods(
                update.episode = function(){
                    sep <- .self$.epindex[index==TRUE]
                    .self$tclean <- merge(.self$tclean, sep, by=c("site", "episode_id"))
                    .self$tclean[["index"]] <- NULL
                })

#' Apply all the setup filters. 
#'
#' Once filters are applied, the processed data will be stored in tclean. Note, 
#' running filtering function before apply_filters is necessary. This function 
#' will have no effect on tclean if no filter is ran prior.
#' Filters will decide to preserve or remove particular entries or episodes. 
#' @param warnings logical value to indicate more or less messages with an 
#' default value TRUE. 
#' @name ccTable_apply_filters
#' @examples
#' \dontrun{
#' tb <- create_cctable(ccd, conf, 1)
#' tb$range_filter() 
#' tb$apply_filter() # apply only the range filter ragardless of the conf. 
#' }
NULL
ccTable$methods(
                apply_filters = function(warnings=TRUE) {
                    "Apply all filters specified in the configuration to update the clean
                    table (tclean)"

                    spec2function <- function(item.name, filter.name) {
                        spec <- .self$conf[[item.name]][[filter.name]]$apply
                        spec <- as.character(as.vector(spec))
                        switch(spec, 
                               "drop_entry"=.self$drop_entry,
                               "drop_episode"=.self$drop_episode,
                               "NA"=function(nmitem, dq){}, 
                               "NULL"=function(nmitem, dq){},
                               stop("functions for applying filters can only be 'drop_entry' or 'drop_episode'. "))
                    }

                    ops <- strsplit(grep('apply', names(unlist(.self$conf)), value=TRUE), "[.]") 
                    for (i in ops) {
                        item <- i[1]
                        filter <- i[2]
                        tryCatch(spec2function(item, filter)(item,
                                                                   .self$dfilter[[filter]]), 
                                 error = function(e) {
                                     if (is.null(.self$dfilter[[filter]])) {
                                         if (warnings)
                                             warning(paste(item, "filter", filter, 
                                                           "has been specified in the configuration but has not been ran."))
                                     }
                                     else {
                                         cat(paste(item, filter, "\n"))
                                         stop(e)
                                     }
                                 })
                    }
                    .self$update.entry()
                    .self$update.episode()
                })

ccTable$methods(
                drop_entry = function(nmitem, dq){
                    .self$.rindex[[nmitem]] <- 
                        .self$.rindex[[nmitem]] & dq$entry[[nmitem]]
                })

ccTable$methods(
                drop_episode = function(nmitem, dq){
                    .self$.epindex[["index"]] <- 
                        .self$.epindex[["index"]] & dq$episode[["select_index"]]
                })

#' Reload the YAML configuration file
#'
#' Note, this function will also reset all the operations and 
#' remove the tclean. 
#' @name ccTable_reload_conf
#' @param conf full path of the YAML configuration file or the parsed config list. 
#' @examples 
#' \dontrun{
#' tb$reload_conf("REF.yaml")
#' }
NULL
ccTable$methods(
                reload_conf = function(conf) {
                    "reload yaml configuration."
                    if (is.character(conf))
                        .self$conf <- yaml.load_file(conf)
                    if (!is.list(conf))
                        stop("conf must be a list or the full path to a YAML file.")
                    else 
                        .self$conf <- conf
                    .self$reset()
                })

#' Reset the ccTable
#' 
#' Restore the object to its initial status. All the filters, quality and the 
#' cleaned table will be removed.
#' @name ccTable_reset
NULL
ccTable$methods(
                reset = function() {
                    .self$dfilter <- list()
                    .self$dquality <- list()
                    .self$tclean <- .self$torigin
                })

#' Export the clean table as a CSV file
#' 
#' Export tclean as a CSV file.
#' @name ccTable_export_csv
#' @param file the full path of the output CSV file. 
NULL
ccTable$methods(
                export_csv = function(file=NULL) {
                    "Export the cleaned table to a CSV file."
                    if (is.null(file))
                        return(.self$tclean)

                    write.csv(.self$tclean, file=file)
                })

#' Apply all the filters
#'
#' All the filters in configuration will be applied to create the 
#' clean dataset. The filters include range, categories, missingness, 
#' no_data. 
#' @name ccTable_clean
#' @examples 
#' \dontrun{
#' tb <- create_cctable(ccd, conf, 1)
#' tb$clean()
#' }
NULL
ccTable$methods(
                clean = function() {
                    if (nrow(.self$torigin) != 0 ) {
                        .self$filter_range()
                        .self$filter_categories()
                        .self$filter_missingness()
                        .self$filter_nodata()
                        .self$apply_filters()
                    }
                    else 
                        warning("The original table is NULL, hence no cleaning process has been performed.")
                })

itemsToDataFrame <- function(ep, items, period_length, freq) {
    listmatrix <- list()
    time <- seq(0, period_length, freq)

    listmatrix[["time"]] <- time

    for (i in items) {
        if (length(ep@data[[i]]) == 1) {
            listmatrix[[i]] <- rep(ep@data[[i]], length(time))
        }
        else {
            if ("time" %in% names(ep@data[[i]])) {
                new <- reallocateTime(ep@data[[i]], period_length, freq)
                listmatrix[[i]] <- new$val
                if ("meta" %in% names(ep@data[[i]]))
                    listmatrix[[paste(i, "meta", sep=".")]] <- new$meta
            }
            else
                listmatrix[[i]] <- rep("NA", length(time))
        }
    }
    return(listmatrix)
}

#' Create the table for ccTable from ccRecord
#' 
#' @param record ccRecord
#' @param items_opt character vectors. Items (HIC code) selected in item_opt are optional items, which will be automatically 
#' filled when item is missing. 
#' @param items_obg obligatory items that is obligatory; Any episode that does not contain
#' item in this vector will be removed.
#' @param freq numeric cadence in hour. 
#' @param return_list logical if TRUE return as a list.  
#' @return data.table
ccd_select_table <- function(record, items_opt=NULL, items_obg=NULL, freq,
                        return_list=FALSE) {

    all_items <- c(items_opt, items_obg)
    if (is.null(all_items))
        stop('both items_opt and items_obg are NULL')

    env <- environment()
    lt <- list()
    stopifnot(is.list(env$lt)) # totally redundent, just to avoid an anonying
    # note says env is assigned but not used!
    for_each_episode(record,
                     function(ep) {
                         if (all(items_obg %in% names(ep@data))) {
                             result <- list()
                             period_length <- getEpisodePeriod(ep)
                             # getEpisodePeriod will return NULL when no 2D
                             # data been found. 
                             if (!is.null(period_length)) {
                                 if (period_length > 0 ) {
                                     result <- append(result,
                                                      itemsToDataFrame(ep, all_items,
                                                                       period_length,
                                                                       freq))
                                     nlength <- length(result[["time"]])
                                     result[["site"]] <- rep(ep@site_id, nlength)
                                     result[["episode_id"]] <- rep(ep@episode_id, nlength)
                                     env$lt[[length(lt) + 1]]<- .simple.data.frame(result)
                                 }
                             }
                         }
                     })
    if (return_list)
        return(lt)

    # fill is true because meta data column can be missing. 
    dt <- rbindlist(lt, fill=TRUE) 


    # Adding missing meta columns to keep the 2d wide consistent. 
    code.has.meta <- names(unlist(sapply(ITEM_REF, function(x) x$NHICmetaCode)))
    for (i in all_items) {
        meta.code <- paste(i, "meta", sep=".")
        if (i %in% code.has.meta & !(meta.code %in% names(dt))) {
            dt[[meta.code]] <- rep("NA", nrow(dt))
        }
    }

    # convert data type 
    for (i in all_items)
        dt[[i]] <- suppressWarnings(.which.datatype(i)(as.character(dt[[i]])))


    return(dt)
}

#' Clean table - low memory
#' 
#' The cleaning process is specified by the YAML configuration. All the filters
#' presented in the configuration will be applied. It returns only the cleaned
#' data. However all the data quality information will be lost. This function
#' is useful when the memory is not sufficiently enough to hold all the
#' information. 
#' @param record ccRecord
#' @param config the full path of the YAML configuration file
#' @param freq table cadence
#' @param nchunks integer number. The larger the nchunks the less memory
#' requirement. 
#' @return A cleaned 2d wide table
#' @export create2dclean
create2dclean <- function(record, config, freq=1, nchunks=1) {
    .create2dclean <- function(record, config, freq) {
        dt.sofa <- create_cctable(rec=record, conf=config, freq=freq)
        dt.sofa$filter_range()
        dt.sofa$filter_categories()
        dt.sofa$filter_missingness()
        dt.sofa$filter.nodata()
        dt.sofa$apply_filters()
        return(dt.sofa)
    }


    if (is.character(config))
        config <- yaml.load_file(config)

    stopifnot(nchunks > 0 & nchunks < record@nepisodes)

    if (nchunks == 1)
        return(.create2dclean(record, config, freq)$tclean)

    op.seq <- round(seq(1, record@nepisodes + 1, length.out=nchunks + 1))

    tclean <- list()

    for (i in seq(length(op.seq) - 1)) {
        rc <- record[seq(op.seq[i], op.seq[i+1] - 1)]
        tclean[[i]] <- .create2dclean(rc, config, freq)$tclean
        gc()
    }

    tclean <- rbindlist(tclean, fill=TRUE)
    return(tclean)
}



#' @importFrom Rcpp evalCpp
#' @useDynLib cleanEHR 
reallocateTime <- function(d, t_discharge, frequency) {
    d_ <- d
    stopifnot(any(names(d) == "time"))
    stopifnot(any(names(d) == "item2d"))
    stopifnot(class(d$time) == "numeric")
    return(reallocateTime_(d_, t_discharge, frequency))
}


findMaxTime <- function(episode) {
    get2dTime <- function(episode){
        time_lt <- 
            lapply(episode@data, 
                   function(item){
                       if(length(item) > 1) {
                           if (!is.numeric(item$time))
                               item$time <-
                                   as.numeric(as.character(item$time))
                               return(max(item$time))
                       }
                   })
        tm <- unlist(time_lt)
        tm
    }
    tm <- get2dTime(episode)
    if (is.null(tm))
        return(NULL)
    else
        return(max(tm))
}


#' Get the length of stay based on the first and the last data point. 
#' 
#' @param e ccEpisode object.
#' @param unit character string.  Units in which the results are desired. Can be abbreviated.
#' @return length of stay
#' @export getEpisodePeriod
getEpisodePeriod <- function (e, unit="hours") {
    # pseudo delta period, see addPseudoTime()
    if (class(e@t_discharge)[1] == "numeric")
        return(e@t_discharge)

    if (class(e@t_admission)[1] != "POSIXct")
        tadm <- xmlTime2POSIX(as.character(e@t_admission), allow=TRUE)
    else 
        tadm <- e@t_admission
    if (class(e@t_discharge)[1] != "POSIXct")
        tdisc <- xmlTime2POSIX(as.character(e@t_discharge), allow=TRUE)
    else 
        tdisc <- e@t_discharge

    # The failure of POSIX conversion indicates that this episode is either 
    # anonymised or has a missing or incorrect value of discharge or admission
    # time. 
    if (is.na(tdisc)) {
      if (is.null(findMaxTime(e))) {
        period_length <- 0
      } else {
        period_length <- ceiling(
          as.numeric(
            difftime(
              as.POSIXct.numeric(findMaxTime(e), origin = "1970-01-01 00:00:00"), tadm, units = unit)
          ))
      }
    } else {
        if (any(is.null(tdisc), is.null(tadm)))
            period_length <- NULL
        else
            period_length <- as.numeric(tdisc - tadm,
                                        units=unit)
    }
    # in cases that tdisc == tadm
    if (!is.null(period_length)) {
        if (period_length == 0)
            period_length <- period_length + 1
    }
    
    

    if (is.null(period_length))
        warning("This episode does not have any time series data: ", 
                " episode_id = ", e@episode_id, 
                " nhs_number = ", e@nhs_number, 
                " pas_number = ", e@pas_number,
                " period_length = ", period_length, "\n")


    return(period_length)
}

#' Propagate a numerical delta time interval record.
#'
#' @param record ccRecord
#' @param delta time frequency in hours
#' @details when discharge time and admission time are missing, the latest  and
#' the earliest data time stamp will be used instead.
reallocateTimeRecord <- function(record, delta=0.5) {
    reallocate.episode <- function(e) {
        env <- environment()
        # make sure admin and disc time is correct
        period_length <- getEpisodePeriod(e)
        if (period_length < 0)  warning("period length < 0")

        # calling reallocateTime for each data item
        new.episode(lapply(e@data, 
                           function(d) {
                               if (length(d) > 1) {
                                   return(reallocateTime(d, env$period_length, delta))
                               } else 
                                   return(d)
                           }))
    }
    newdata <- for_each_episode(record, reallocate.episode)
    return(ccRecord() + newdata)
}
