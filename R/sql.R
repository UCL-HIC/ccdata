#' Extract a variable from ccRecord object to a table.
#' 
#' The output table will contain site_id, episode_id, time, val, and possible
#' meta data column if the variable is longitudinal. Otherwise only site_id,
#' episode_id and the value will be presented. 
#' @param ccd ccRecord object 
#' @param varname the short name of the selected variable, see ITEM_REF
#' @export 
rec2tb <- function(ccd, varname) {

    varcode <- stname2code(varname)
    if (is.na(names(stname2code(varname)))) 
        stop("The short name ", varname, " cannot be found.")

    vartb.lst <- 
        for_each_episode(ccd, 
        function(x) {
            if (is.null(x@data[[varcode]]))
                return(NULL)
            else 
                return(cbind(.simple.data.frame(
                      list(episode_id=x@episode_id, 
                           site_id=x@site_id)),
                      x@data[[varcode]]))
        })
    vartb <- rbindlist(vartb.lst, fill=TRUE)
    nm <- names(vartb)

    # Rename the columns 
    # The sequence of time and item2d column is not certain, that's why we have
    # to search the index of item2d first. 
    if (ncol(vartb) == 3) # non-longitudinal variables
        names(vartb) <- c("episoe_id", "site_id", varname)
    if (ncol(vartb) > 3) # longitudinal 
        names(vartb) <- gsub(x=nm, "item2d", "val")
    # two val may happen for microb site. 
    if (any(duplicated(nm))) {
        nm[duplicated(nm)] <- paste0(nm[duplicated(nm)], 1)
        names(vartb) <- nm
    }
    return(vartb)
}


#' Export all the longitudinal fields as tables 
#'
#' All the result tables will be wrapped by a list, indexing with short names of
#' the fields. 
#' @param ccd ccRecord object
#' @export
export.lontb <- function(ccd) {
    l.stnames <- code2stname(names(ITEM_REF)[is.longitudinal(names(ITEM_REF))])
    names(l.stnames) <- l.stnames # the result can be indexed via short names
    return(lapply(l.stnames, function(x) rec2tb(ccd, x)))
}


#' @importFrom dplyr src_sqlite copy_to
#' @import RSQLite
#' @export
sql.create.database <- function(ccd, path="cchic.sqlite3") {
    unlink(path)
    cchic_db <- src_sqlite(path, create = T)
    ltb <- export.lontb(ccd)

    for (i in seq(ltb)) {
        data = ltb[[i]] # copy_to does not like [[]] operators for df.
        name = names(ltb[i])
        if (nrow(data) != 0) {
            copy_to(dest=cchic_db, df=data, name=name, temporary = FALSE)
        }
        else 
            warning("cannot find any data of ", name, "in ccd.")
    }
    tb <- suppressWarnings(as.data.frame(sql.demographic.table(ccd)))
    copy_to(dest=cchic_db, df=tb, name="episodetb", temporary = FALSE)

    cchic_db
}

#' @importFrom dplyr sql
#' @export
sql.tbl.vartb <- function(con, stname) {
    tbl(con, sql(paste("select * from", stname)))
}


#' @importFrom dplyr collect tbl
#' @export 
sql.collect.vartb <- function(con, stname) {
    return(collect(sql.tbl.vartb(con, stname), n = Inf))
}


#' Fat table 
#' 
#' We should be able to give the users choices of how to align the data, whether by 
#' using ICU admission time as the baseline, or hospital admission, or even with the earliest 
#' and the latest data point. I have updated reallocateTime__ which allows such 
#' operations. 
#'
#' @importFrom dplyr src_tbls
#' @export 
create.fat.table <- function(db, frequency=1) {
    tbindb <- src_tbls(db)
    # clean up fat table and tmp table in the database  
    for (i in c("fattb", "tmp", "vartb")) {
        if (i %in% tbindb)
            db$con %>% db_drop_table(table=i)
    }
    ltbname <- tbindb[sapply(src_tbls(con), function(x) grep(x=x, pattern="^l_")) == 1]
    ltbname <- ltbname[!is.na(ltbname)]

    for (i in ltbname) db$con %>% db_drop_table(i)

    eptb <- data.table(sql.collect.vartb(db, 'episodetb'))
    eptb <- data.table(lenstay(eptb))
    nep0 <- nrow(eptb)
    eptb <- eptb[!is.na(lenstay)]
    eprm.log(nep0, nrow(eptb), "unable to calculate length of stay.")
    eptb <- eptb[, c("ICNNO", "ADNO", "lenstay"), with=FALSE]
    names(eptb) <- c("site_id", "episode_id", "lenstay")

    fatbasetb <- eptb[, seq(0, ceiling(as.numeric(lenstay))), by=c("site_id", "episode_id")]
    names(fatbasetb) <- c("site_id", "episode_id", "time")
    fatbasetb$episode_id <- as.character(fatbasetb$episode_id) # this is a patch, should keep the consistency of the data type everywehre. 
    copy_to(db, fatbasetb, 'fattb', temporary=FALSE)


#    lonvars <- c('adrenaline', 'advsupt_cardv', "h_rate")
    lonvars <- names(ITEM_REF)[is.longitudinal(names(ITEM_REF))]
    lonvars <- code2stname(lonvars)
    
    if (any(!(lonvars %in% tbindb)))
        warning(paste(lonvars[!(lonvars %in% tbindb)], collapse=", "),
                      ", cannot be found in the database.")
    lonvars <- lonvars[lonvars %in% tbindb]
    

    for (l in lonvars) {
        print(l)
        var <- data.table(sql.collect.vartb(db, l))
        var <- var[, alignTime(.SD, 0, max(time), 1), by=c("site_id", "episode_id")]
        var <- merge(fatbasetb, var, by=c("site_id", "episode_id", "time"), all.x=TRUE)
        print(var)
        copy_to(db, var, paste0('l_', l), temporary=FALSE)
    }
}

create.index <- function(dbname, tbname) {
    con <- dbConnect(drv = SQLite(), dbname=dbname)
    dbSendQuery(con, 
                paste0("create index ind on ", 
                       tbname, " (site_id, episode_id, time);"))

    dbClearResult(dbListResults(con)[[1]])
    dbDisconnect(con)
}


#' Join the existing vartb and fattb and create update the new fattb
#' 
#' Update the fattb and the vartb will be removed here.
#' @param dbname character database address
left.join.var.table <- function(dbname) {
    con <- dbConnect(drv = SQLite(), dbname=dbname)
    # Get the exact column names to avoid duplications in join, such as site_id.1
    basecolname <- names(dbGetQuery(con, "select * from fattb limit 1;"))
    varcolname <- names(dbGetQuery(con, "select * from vartb limit 1;"))
    varcolname <- varcolname[-which(varcolname %in% c('site_id', 'episode_id', 'time'))]
    newcolname <- c(paste0("fattb.", basecolname), varcolname)

    q.jointable  <- 
        paste0("CREATE TABLE tmp as SELECT ", paste(newcolname, collapse=", "), 
               " FROM fattb LEFT OUTER JOIN vartb ON 
               fattb.site_id = vartb.site_id and 
               fattb.episode_id = vartb.episode_id and 
               fattb.time = vartb.time;")

    dbSendQuery(con, q.jointable)
    dbSendQuery(con, "DROP TABLE fattb;")
    dbSendQuery(con, "ALTER TABLE tmp RENAME TO fattb;")
    dbSendQuery(con, "DROP TABLE vartb;")

    dbClearResult(dbListResults(con)[[1]])
    dbDisconnect(con)
}

#' @export 
eprm.log <- function(nep0, nep1, reason) {
    cat("[-]", 
        nep0 - nep1, 
        "episodes have been removed:", 
        reason, "\n")
}


#' @export 
get.lvar <- function(con, var) {
    as.data.frame(tbl(con, sql(paste("select val from", paste0("l_", var)))))
}
