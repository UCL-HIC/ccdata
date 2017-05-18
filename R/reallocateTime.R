#' @importFrom Rcpp evalCpp
#' @useDynLib cleanEHR
#' @export 
reallocateTime <- function(d, t_discharge, frequency) {
    d_ <- d
    stopifnot(any(names(d) == "time"))
    stopifnot(any(names(d) == "val"))
    stopifnot(class(d$time) == "numeric")
    return(reallocateTime_(d_, t_discharge, frequency))
}

#' @importFrom Rcpp evalCpp
#' @export
alignTime <- function(d, t_admission, t_discharge, frequency) {
    stopifnot(any(names(d) == "time"))
    stopifnot(any(names(d) == "val"))
    stopifnot(class(d$time) == "numeric")
    return(reallocateTime__(d, t_admission, t_discharge, frequency))
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
        tadm <- xmlTime2POSIX(as.character(e@t_admission), allow=T)
    else 
        tadm <- e@t_admission
    if (class(e@t_discharge)[1] != "POSIXct")
        tdisc <- xmlTime2POSIX(as.character(e@t_discharge), allow=T)
    else 
        tdisc <- e@t_discharge

    # The failure of POSIX conversion indicates that this episode is either 
    # anonymised or has a missing or incorrect value of discharge or admission
    # time. 
    if (is.na(tadm) || is.na(tdisc))
        period_length <- findMaxTime(e)
    else {
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
#' @param record ccRecord
#' @param delta time frequency in hours
#' @details when discharge time and admission time are missing, the latest  and
#' the earliest data time stamp will be used instead.
#' @export reallocateTimeRecord
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
