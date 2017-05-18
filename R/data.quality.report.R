#' Create the data quality report
#'
#' Create a detailed data quality report, including file summary, site 
#' summary, data completeness, and density plot. The result can be found 
#' in {work_dir}/report/data_quality_report.{pdf}/{md}. Using this function, 
#' one can also create a site/trust specified report, see the argument "site". 
#' You need to make sure that you have the right to write into the {work_dir}. 
#' 
#' @param ccd ccRecord 
#' @param site a vector of the site ids for the site specified report. 
#' @param pdf logical create the pdf version of the DQ report, 
#' otherwise stay in markdown format
#' @param file charcter a list of XML file origins. 
#' @param out character output path
#' @export data.quality.report
#' @examples 
#' \dontrun{data.quality.report(ccd, c("Q70", "C90"))}
#' @import knitr
#' @import pander
#' @import ggplot2
data.quality.report <- function(ccd, site=NULL, file=NULL, pdf=T, out="report") {
#    if (is.null(site) & is.null(file))  dbfull <- "YES"
#    else dbfull <- "NO"
    
    stopifnot(!(!is.null(site) & !is.null(file)))
    if (!is.null(site)) ccd <- ccd[site]
    if (!is.null(file)) ccd <- ccRecord_subset_files(ccd, file)
    
 
    if (dir.exists(out)) {
        unlink(out, recursive=T)
    }
    dir.create(out)

    wd <- getwd()
    rptpath <- paste(path.package('cleanEHR'), "report", sep="/")
    file.copy(dir(rptpath, full.names=T), out, recursive=T)

    write.report <- function() {
        setwd(out)
        dqpath <- "data_quality_report.Rmd"
        headerpath <- "listings-setup.tex"
        tpltpath <- "report.latex"

        knit(dqpath, "data_quality_report.md")
        if (pdf) {
            pandoc.cmd <- 
                paste("pandoc -s -N --toc --listings -H ", headerpath,
                      " --template=", tpltpath, 
                      " -V --number-section  -V papersize:a4paper -V geometry:margin=1.3in ", 
                      "data_quality_report.md -o data_quality_report.pdf", sep="")
            tryCatch(system(pandoc.cmd), 
                     error = function(e) {
                         cat(e)
                         setwd(wd)
                     }, 
                     finally = {
                         setwd(wd)
                     })
            setwd(wd)
        }
    }

    tryCatch(write.report(), finally={setwd(wd)})

}


#' Create the data quality report
#'
#' Create a detailed data quality report, including file summary, site 
#' summary, data completeness, and density plot. The result can be found 
#' in {path}/report/data_quality_report.{pdf}/{md}. Using this function, 
#' one can also create a site/trust specified report, see the argument "site". 
#' You need to make sure that you have the right to write into the {work_dir}. 
#' 
#' @param ccd ccRecord 
#' @param pdf logical create the pdf version of the DQ report, 
#' otherwise stay in markdown format
#' @param brc character BRC names which can be Cambridge, GSTT, Imperial,
#' Oxford, and UCLH.  
#' @param path report export path 
#' @export data.quality.report.brc 
data.quality.report.brc <- function(ccd, pdf=T, brc=NULL, path=NULL) {
    if (!is.null(path))
        dir.create(path)

    if (is.null(brc)) brc <- c("Cambridge", "GSTT", "Imperial", "Oxford", "UCLH")
    
    xmlfiles <- unique(ccd@infotb$parse_file)

    data.quality.report(ccd, pdf=pdf, out=paste(path, "report_full_db", sep="/"))

    for (i in brc) {
        fs <- xmlfiles[grepl(i, xmlfiles)]
        if (length(fs) > 0)
            data.quality.report(ccd, pdf=pdf, file=fs, 
                                out=paste(path, paste0("report_", i), sep="/"))
        else
            cat("No XML files from trust ", i, " has been found.", '\n')
    }
}

#' Produce a file summary table
#' 
#' @param ccd ccRecord-class
#' @return data.table
#' @export file.summary
file.summary <- function(ccd) {
    infotb <- ccd@infotb
    file.summary <- infotb[, list("Number of Episodes"=.N, 
                                  "Upload time"=max(.SD[["parse_time"]]), 
                                  "Sites"=paste(unique(.SD[["site_id"]]),
                                                collapse=", ")), by="parse_file"]
    file.summary[["File"]] <- file.summary$parse_file
    file.summary[["parse_file"]] <- NULL
    return(file.summary)
}

#' Plot the XML duration in terms of sites. 
#'
#' @param ccd ccRecord-class
#' @export xml.site.duration.plot
xml.site.duration.plot <- function(ccd) {
    tb <- copy(ccd@infotb)
    tb <- tb[, list("minadm"=min(.SD[["t_admission"]], na.rm=T), 
              maxadm=max(.SD[["t_admission"]], na.rm=T),
              mindis=min(.SD[["t_discharge"]], na.rm=T),
              maxdis=max(.SD[["t_discharge"]], na.rm=T)), by="site_id"]
    site_name <- apply((site.info()[tb$site_id, ][,1:2]), 1, 
          function(x) paste(x, collapse="-"))
    tb[, site_name:=site_name]
    
    ggplot(tb, aes_string(x="minadm", y="site_name")) +
        geom_segment(aes_string(xend="maxdis", yend="site_name"), color="gray", size=10) +
        annotate("text", x=tb$minadm+(tb$maxdis-tb$minadm)/2, 
                 y=tb$site_name, label=tb$site_name, size=7) + 
        scale_x_datetime(date_breaks="3 month")+
        theme(axis.text.y=element_blank()) + 
        ggtitle("Site") +
        xlab("") + ylab("")
}

#' plot the duration of XML files. 
#'
#' @param ccd ccRecord-class
#' @export xml.file.duration.plot
xml.file.duration.plot <- function(ccd) {
    tb <- copy(ccd@infotb)
    tb <- tb[, list(minadm=min(.SD[["t_admission"]], na.rm=T), 
              maxadm=max(.SD[["t_admission"]], na.rm=T),
              mindis=min(.SD[["t_discharge"]], na.rm=T),
              maxdis=max(.SD[["t_discharge"]], na.rm=T)), by="parse_file"]
    ggplot(tb, aes_string(x="minadm", y="parse_file")) +
        geom_segment(aes_string(xend="maxdis", yend="parse_file"), color="gray", size=10) +
        annotate("text", x=tb$minadm+(tb$maxdis-tb$minadm)/2, 
                 y=tb$parse_file, label=tb$parse_file, size=7) + 
        scale_x_datetime(date_breaks="3 month")+
        theme(axis.text.y=element_blank()) + 
        ggtitle("The Duration of XML Files") +
        xlab("") + ylab("")
}



txt.color <- function(x, color) {
    x <- sprintf("%3.2f", x)
    paste("\\colorbox{", color, "}{", x, "}", sep="")
}

#' Create a demographic completeness table (in pander table)
#' 
#' @param demg data.table the demographic data table created by sql.demographic.table()
#' @param names short name of selected items
#' @param return.data logical return the table if TRUE
#' @export demographic.data.completeness
demographic.data.completeness <- function(demg, names=NULL, return.data=FALSE) {
    site.reject <- function(demg, name, ref) {
        if (ref == 0 | name == "ICNNO")
            return("")
       stb <- 
            demg[, 
                 round(length(which(!is.na(get(name)) & 
                                    get(name)!="NULL"))/.N * 100, 
                       digits=2), by="ICNNO"]
        rej <- stb[stb[[2]] < ref]
        if (nrow(rej) == 0)
            return("")
        else
            return(paste(apply(rej, 1, function(x) paste(x, collapse=":")),
                  collapse="; "))
    }

    acpt <- unlist(yaml.load_file(system.file("conf/accept_completeness.yaml", 
                                              package="cleanEHR")))

 
    demg <- copy(demg)
    demg[, "index":=NULL]
    if (!is.null(names))
        demg <- demg[, names, with=F]

    cmplt <- apply(demg, 2, function(x) length(which(!(x=="NULL" | is.na(x)))))
    cmplt <- data.frame(cmplt)
    cmplt[, 1] <- round(cmplt[, 1]/nrow(demg)*100, digits=2)

    ref <- acpt[rownames(cmplt)]
    stopifnot(all(!is.na(ref)))
    vals <- cmplt[, 1]
    stname <- rownames(cmplt)
    
    reject <- array("", length(stname))
    for (i in seq(nrow(cmplt))) { 
        reject[i] <- site.reject(demg, stname[i], ref[i])
    }

    # color the text according the reference
    ind <- vals >= ref & ref != 0
    cmplt[, 1][ind] <- txt.color(vals[ind], "ccdgreen")
    ind <- vals < ref & ref != 0
    cmplt[, 1][ind] <- txt.color(vals[ind], "ccdred")

    hiccode <- stname2code(rownames(cmplt))
    lname <- stname2longname(rownames(cmplt))
    rownames(cmplt) <- lname 
    cmplt$ref <- as.character(ref)
    cmplt$ref[cmplt$ref=="0"] <- ""
    cmplt$reject <- reject

    cmplt <- cbind(cmplt, as.number(StdId(hiccode)))

    names(cmplt) <- c("Completeness %", "Accept Completeness %", 
                      "Rejected Sites (Site: %)", "NHIC Code (NIHR_HIC_ICU_xxxx)")
    
    if (return.data)
        return(cmplt)
    pander(as.data.frame(cmplt), style="rmarkdown", justify = c('left', 'center', "center",
                                                 "center", 'left'))
}

#' Produce a pander table of sample rate of longitudinal data.
#'
#' @param cctb ccTable-class, see create.cctable().  
#' @export samplerate2d
samplerate2d <- function(cctb) {
    sample.rate.table <- data.frame(fix.empty.names=T)
    # items are the columns before site.  
    items <- names(cctb)[-c(grep("meta", names(cctb)), 
                            which(names(cctb) %in% 
                                  c("site", "time", "episode_id")))]
    for (i in items) {
        sr <- nrow(cctb)/length(which(!is.na(cctb[[i]])))
        sample.rate.table <- 
            rbind(sample.rate.table, 
                  data.frame("item"=stname2longname(code2stname(i)), 
                             "hiccode"=as.number(StdId(i)),
                             "sr"=sr))
    }
    rownames(sample.rate.table) <- NULL
    
    names(sample.rate.table) <- c("Item", "NHIC Code (NIHR_HIC_ICU_xxxx)", 
                                  "Sample Period (hour)")

    pander(as.data.frame(sample.rate.table), style="rmarkdown")
}



#' Return total data point of the ccRecord object. 
#'
#' @param ccd ccRecord-class
#' @export total.data.point
total.data.point <- function(ccd) {
    dp.physio <- 
        sum(unlist(for_each_episode(ccd, 
                                    function(x) 
                                        Reduce(sum, sapply(x@data, nrow)))))
    dp.demg <-
        sum(unlist(for_each_episode(ccd, 
                                    function(x) 
                                        Reduce(sum, sapply(x@data, nrow)))))
    return(sum(dp.physio, dp.demg))
}

#' Produce the item specified table one. 
#'
#' @param demg demographic table created by sql.demographic.table()
#' @param names character string. Short names of data items, e.g. h_rate. 
#' @param return.data logical, FALSE: printing the pander table, TRUE: return the table but not print out the pander table. 
#' @return if return.data is TRUE, return data.table
#' @export table1
table1 <- function(demg, names, return.data=FALSE) {
    panderOptions('knitr.auto.asis', FALSE)

    if (!return.data)
        cat(paste("\n## Table ONE\n"))
    table1.item <- function(demg, name) {
        ref <- ITEM_REF[[stname2code(name)]]
        hicnum <- as.number(StdId(stname2code(name)))
        if (is.null(ref))
            stop("The short name cannot be found in ITEM_REF.")
        if (!return.data)
            cat(paste("\n###", ref$dataItem," - ", hicnum, "\n"))
        if (ref$Datatype %in% c("text", "list", "Logical", "list / Logical")) {
            stopifnot(!is.null(ref$levels))
            nmref <- sapply(ref$levels, function(x) x)
            r <- demg[, .N, by=name]
            level.name <- nmref[r[[name]]]
            r[, "nm":=level.name]
            r[, "percent":=.SD[["N"]]/nrow(demg)*100]

            tb <- data.table(
                              "Category"=r$nm,
                              "Episode Count"=r$N,  
                              "Percentage"=paste(round(r$percent, digits=1), "%"))
            setkey(tb, "Episode Count")

        }
        else stop(name, "is not a categorical variable.")
        if (return.data)
            return(tb)
        else 
            pander(as.data.frame(tb), style="rmarkdown")
    }

    for (i in names)
        table1.item(demg, i)

    panderOptions('knitr.auto.asis', TRUE)
}


#' demg.distribution
#' Create a plot of the distribution of numerical demographic data.
#' 
#' @param demg ccRecord or demographic table created by sql.demographic.table()
#' @param names character vector of short names of numerical demographic data. 
#' @examples
#' \dontrun{tdemg.distribution(ccd, "HCM")}
#' @export demg.distribution
demg.distribution <- function(demg, names) {
    if (class(demg) == "ccRecord")
        demg <- sql.demographic.table(demg)
    for (nm in names) {
        ref <- ITEM_REF[[stname2code(nm)]]
        hicnum <- as.number(StdId(stname2code(nm)))
        cat(paste("\n\n###", ref$dataItem, " - ", hicnum, "\n"))
        gg <- ggplot(demg, aes_string(nm)) + geom_density(fill="lightsteelblue3") + 
            facet_wrap(~ICNNO, scales="free")
        print(gg)
        cat('\\newpage')
    }
}

#' Plot the physiological data distribution.
#'
#' @param cctb ccTable-class, see create.cctable().  
#' @param names character vector of short names of numerical demographic data. 
#' @export physio.distribution
physio.distribution <- function(cctb, names) {
    for (nm in names) {
        ref <- ITEM_REF[[stname2code(nm)]]
        hicnum <- as.number(StdId(stname2code(nm)))
        cat(paste("\n\n###", ref$dataItem, "-", hicnum, "\n"))
        gg <- ggplot(cctb, aes_string(ref$NHICcode)) + geom_density(fill="lightsteelblue3") + 
            facet_wrap(~site)
        print(gg)
        cat('\\newpage')
    }
}
