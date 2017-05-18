make_equal_length <- function(lt) {
    len <- unlist(sapply(lt, length))
    if (is.null(len))
        return(lt)

    if (length(unique(len)) == 1) {
        return(lt)
    }
    else{
        maxlen <- max(len)
        return(lapply(lt, function(x) c(x, rep("NA", maxlen-length(x)))))
    }
}


xmlEpisodeToList <- function(episode_node) {
    traverseNode <- function(node) {
        if (is.null(node)) {
            #leaf node reached. Turn back
            return()
        }

        num.children = xmlSize(node)  
        if(num.children == 0 ) {
            label <- xmlName(xmlParent(node))
            if (is.null(.which.type(label))) {
                warning("XML structure error:", label)
            }
            else {
                if (.which.type(label) == "item1d") {
                    node_env$data[[label]][[label]] <- xmlValue(node)
                }

                else {
                    parent_size <- xmlSize(xmlParent(xmlParent(node)))
                    sib <- getSibling(xmlParent(node))

                    if (parent_size == 2) {
                        if (!is.null(sib)) {
                            old_data <- node_env$data[[label]][[label]]

                            if(is.null(old_data))
                                node_env$data[[label]][[label]] <- vector()
                            
                            node_env$data[[label]][[label]] <- c(old_data, xmlValue(node))

                            old_sibling <- node_env$data[[label]][[xmlName(sib)]]

                            if (is.null(old_sibling))
                                node_env$data[[label]][[xmlName(sib)]] <- vector()
                                
                            node_env$data[[label]][[xmlName(sib)]] <- c(old_sibling, xmlValue(sib))
                        }
                    }
                    else if (parent_size == 3) {
                        if (!is.null(sib)){
                            sibsib <- getSibling(sib)
                            if (!is.null(sibsib)) {
                                sibling1_name <- xmlName(sib)
                                sibling2_name <- xmlName(sibsib)

                                node_env$data[[label]] <- make_equal_length(node_env$data[[label]])

                                old_data <- node_env$data[[label]][[label]]
                                old_sibling1 <- node_env$data[[label]][[sibling1_name]]
                                old_sibling2 <- node_env$data[[label]][[sibling2_name]]

                                if (is.null(old_sibling1))
                                    node_env$data[[label]][[sibling1_name]] <- vector()
                                if (is.null(old_sibling2))
                                    node_env$data[[label]][[sibling2_name]] <- vector()


                                node_env$data[[label]][[label]] <- c(old_data, xmlValue(node))
                                node_env$data[[label]][[sibling1_name]] <- c(old_sibling1, xmlValue(sib))
                                node_env$data[[label]][[sibling2_name]] <- c(old_sibling2, xmlValue(sibsib))
                            }
                        }
                    }

                }
            }
        }

        #Go one level denode_env$dataer
        for (i in 1 : num.children) {
            if (xmlValue(node) == "") {
                warning("XML structure is wrong in ", xmlName(node))
                next
            }
            traverseNode(node[[i]]) #the i-th child of node
        }
    }

    node_env <- new.env()
    node_env$data <- list() # store data from the XML traverser 
    traverseNode(episode_node)
    node_env$ccdata <- list() # format can be directely called by ccEpisode.
    
    # rearrange the vector data from node_env$data to ccdata format.
    # Regarding to the performance, XML traverser only put all the data in a
    # vector form, as vector appending is much faster than data.frame
    # appending.
    lapply(node_env$data, function(x) {
               len <- length(x)
               if (len == 1) { # 1d item (simple item)
                   if (.which.type(names(x)) == "item1d" & is.character(x[[1]]))
                       node_env$ccdata[[names(x)]] <- as.character(x)
                   else
                       stop("wrong simple data", names(x))
               }
               else if (len == 2) { # 2d item (items in time)
                   nm <- c(.which.type(names(x)[1]), .which.type(names(x)[2]))
                   label <- names(x)[nm == "val"]
                   if (length(label) == 1) {
                       node_env$ccdata[[label]] <- .simple.data.frame(x)
                       names(node_env$ccdata[[label]]) <- nm
                   }
               }
               else if (len == 3) { # time data with meta data, i.e. 3 columns
                   # list has the same length. In the case that 3 columns
                   # have missing column in the end, so putting NAs in the end.
                   x <- make_equal_length(x)                    
                   
                   nm <- c(.which.type(names(x)[1]), .which.type(names(x)[2]),
                           .which.type(names(x)[3]))
                   label <- names(x)[nm == "val"]
                   # usually label, i.e. val should be unique, however just
                   # in case of incomplete 4-column data in which more than 1
                   # val will be found.
                   for (i in label) {
                       node_env$ccdata[[i]] <- .simple.data.frame(x)
                       names(node_env$ccdata[[i]]) <- nm
                   }
               }
               # still wrong and keep it as it is for now.
               # 4 columns case only happens in laboratory microbiology
               # culture, where item labels 0186 (Site), 0187 (Organism) 
               # and 0189 (Sensitivity) share the same time label and being
               # allocated in the same XML block. In `ccdata` a 4 columns data
               # frame will be created and duplicated under the item names. 
               else if (len == 4) {
                   nm <- c(.which.type(names(x)[1]), .which.type(names(x)[2]), 
                           .which.type(names(x)[3]), .which.type(names(x)[4]))
                   label <- names(x)[nm == "val"]
                   for(i in label) {
                       node_env$ccdata[[i]] <- data.frame(x[[1]], x[[2]],
                                                          x[[3]], x[[4]])
                       names(node_env$ccdata[[i]]) <- nm
                   }
               }
               else{
                   print(x)
                   stop("0 or more than 4 columns here!")
               }
})
    return(node_env$ccdata)
}


#' load xml clinical data
#'
#' @param file character string. The path of the XML file.
#' @return the root of the xml data. 
xmlLoad <- function(file) {
    file.parse <- xmlParse(file)
    xml.root <- xmlRoot(file.parse)
    return(xml.root)
}

#' get the episode data from xml 
#'
#' @param xml.root root of xml data returned by xmlLoad()
#' @param id integer
getXmlepisode <- function(xml.root, id) {
    xml.root[[1]][[2]][[id]]
}

#' Extract the original file name from a path and file removing
#' all the suffixes.
#'
#' @param pathfile a particular file name which may have a suffix
#' @param removestr last bit from the original filename
#' @return string
extract_file_origin <- function(pathfile, removestr='.xml'){
  split_path <- unlist(strsplit(pathfile, "/"))
  filename <- split_path[length(split_path)]
  original <- unlist(strsplit(filename, removestr))
  return(paste(original[1], removestr, sep=""))
  }


#' Convert the XML file to ccRecord 
#' 
#' Convert the XML file to ccRecord. For more details, see ccRecord-class. 
#' 
#' @param file character string. The path of XML file. 
#' @param select.episode integer vector. Load only a selected number of episodes. 
#' It is NULL by default which loads all the episodes in a file. 
#' @param quiet logical. Switch on/off the progress bar. 
#' @param xml XML object. Usually not needed. 
#' @param file_origin character string. The XML file name. The file name will be 
#' extracted automatically while argument xml is NULL. 
#' @param parse_time POSIXct. By default is the time of the execution of this function. 
#' @return ccRecord-class 
#' @export xml2Data
xml2Data <- function (file, select.episode=NULL, quiet=TRUE, xml=NULL,
                      file_origin="NA", parse_time=Sys.time()){
  if (is.null(xml)) {
    if (file_origin == "NA") {
      file_origin <- extract_file_origin(file)
    }
    xml <- xmlLoad(file)
  }

    episode.num <- xmlSize(xml[[1]][[2]])
    if(is.null(select.episode))
        select.episode <- seq(episode.num)


    if (!quiet)
        pb <- utils::txtProgressBar(min = min(select.episode)-1, 
                             max = max(select.episode), style = 3)
    eps <- list()
    if (!quiet)
        cat("parsing XML file: ", file, "\n")

    for(episode.id in select.episode){
        episode <- getXmlepisode(xml, episode.id)
        episode_list <- tryCatch(xmlEpisodeToList(episode), 
                                 error=function(err) {
                                     cat(paste(err, "episode.id = ", episode.id, "\n"))
                                     stop()
                                 })
        eps[[episode.id]] <- new.episode(episode_list, file_origin, parse_time)

        if (!quiet)
            utils::setTxtProgressBar(pb, episode.id)
    }

    record <- ccRecord() + eps
    if (!quiet)
        cat("\n")
    
    return(record)
}
