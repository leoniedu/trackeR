#' Read a training file in tcx, gpx, db3, fit or Golden Cheetah's JSON
#' format
#'
#' @param file The path to a tcx, gpx, json or db3 file. Compressed
#'     versions (gz, bz2, xz, zip) of tcx, gpx, and json files are
#'     directly supported.
#' @param timezone The timezone of the observations as passed on to
#'     \code{\link[base]{as.POSIXct}}.  Ignored for JSON files.
#' @param speedunit Character string indicating the measurement unit
#'     of the speeds in the container file to be converted into meters
#'     per second. See Details.
#' @param distanceunit Character string indicating the measurement
#'     unit of the distance in the container file to be converted into
#'     meters. See Details.
#' @param sport Character string indicating the sport type. If NULL (default),
#'     attempts to identify the sport from the data or filename.
#' @param ... Currently not used.
#'
#' @details
#'
#' Available options for \code{speedunit} currently are
#' \code{km_per_h}, \code{m_per_s}, \code{mi_per_h},
#' \code{ft_per_min} and \code{ft_per_s}. The default is
#' \code{m_per_s} for TCX files and \code{km_per_h} for db3 and
#' Golden Cheetah's json files.  Available options for
#' \code{distanceunit} currently are \code{km}, \code{m},
#' \code{mi} and \code{ft}. The default is \code{m} for TCX and
#' \code{km} for gpx, db3 and Golden Cheetah's json files.
#'
#' \code{readTCX}, \code{readGPX}, \code{readGPX} and \code{readDB3},
#' try to identify the sport from the data in the container file. If
#' that fails, then an attempt is made to guess the sport from
#' keywords in the filename. If identification is not possible then
#' the \code{file} attribute of the returned object has value
#' \code{NA}.
#'
#' @export
#' @name readX
#' @examples
#' ## read raw data
#' filepath <- system.file("extdata/tcx", "2013-06-08-090442.TCX.gz", package = "trackeR")
#' run0 <- readTCX(file = filepath, timezone = "GMT")
#'
#' ## turn into trackeRdata object
#' units0 <- generate_units()
#' run0 <- trackeRdata(run0, units = units0)
#'
#' ## alternatively
#' \dontrun{
#' run0 <- read_container(filepath, type = "tcx", timezone = "GMT")
#' }
#'
#' @export
readTCX <- function(file,
                    timezone = "",
                    speedunit = "m_per_s",
                    distanceunit = "m",
                    sport = NULL,
                    ...) {

    doc <- safe_read_xml(file)
    ns <- xml_ns(doc)

    children_names <- function(x, xpath, ns) {
        unique(xml_name(xml_children(xml_find_all(x, xpath, ns))))
    }

    ## Core namespaces
    activity_ns <- names(which(ns == "http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2")[1])
    ## https://www8.garmin.com/xmlschemas/ActivityExtensionv2.xsd
    ## https://www8.garmin.com/xmlschemas/ActivityExtensionv1.xsd
    extensions_ns <- c("http://www.garmin.com/xmlschemas/ActivityExtension/v2",
                       "http://www.garmin.com/xmlschemas/ActivityExtension/v1")
    extensions_ns <- na.omit(sapply(extensions_ns, function(e) names(which(ns == e)[1])))

    ## Guess sport from data
    if (is.null(sport)) {
        sport <- guess_sport(xml_attr(xml_find_first(doc, paste0("//", activity_ns, ":", "Activity")), "Sport"))
        ## If not successful, try filename
        if (is.na(sport)) {
            sport <- guess_sport(basename(file))
        }
    }

    ## Tp
    tp_xpath <- paste0("//", activity_ns, ":", "Trackpoint")
    ch_names <- children_names(doc, tp_xpath, ns)
    if (length(ch_names) == 0) {
        stop("No usable data have been found in ", file)
    }
    tp_vars <- data.frame(name = ch_names,
                          ns = activity_ns)

    ## Position
    position_xpath <- paste0("//", activity_ns, ":", "Position")
    ## Add any nested fields here
    is_position <- tp_vars$name == "Position"
    if (any(is_position)) {
        ## remove position
        tp_vars <- tp_vars[!is_position, ]
        ## Add longitude/latitude
        children <- data.frame(name = children_names(doc, position_xpath, ns[activity_ns]),
                               ns = activity_ns)
        tp_vars <- rbind(tp_vars, children)
    }

    ## Extensions
    is_extensions <- tp_vars$name == "Extensions"
    if (any(is_extensions)) {
        ## remove position
        tp_vars <- tp_vars[!is_extensions, ]
        for (e in extensions_ns) {
            e_xpath <- paste0("//", e, ":", "TPX")
            ## Add any extensions
            ch_nam <- children_names(doc, e_xpath, ns[e])
            if (length(ch_nam)) {
                children <- data.frame(name = ch_nam, ns = e)
                tp_vars <- rbind(tp_vars, children)
            }
        }
    }
    is_time <- tp_vars$name == "Time"

    tps <- xml_find_all(doc, tp_xpath, ns[activity_ns])
    ## Double loop to extract obs
    observations <- apply(tp_vars, 1, function(var) {
        c_xpath <- paste0(".", "//", var["ns"], ":", var["name"])
        c_ns <- ns[var["ns"]]
        sapply(tps, function(x) {
            xml_text(xml_find_first(x, c_xpath, c_ns))
        })
    })

    observations <- as.data.frame(observations, stringsAsFactors = FALSE)

    names(observations) <- tp_vars$name

    ## Convert to numeric
    observations[!is_time] <- apply(observations[!is_time], 2, as.numeric)

    ## human names
    allnames <- generate_variable_names()
    namesOfInterest <- allnames$tcx2_names
    namesToBeUsed <- allnames$human_names
    inds <- match(namesOfInterest, names(observations), nomatch = 0)
    observations <- observations[inds]
    names(observations) <- namesToBeUsed[inds!=0]

    ## coerce time into POSIXct
    observations$time <- gsub("[\t\n]", "", observations$time)
    observations$time <- convertTCXTimes2POSIXct(observations$time, timezone = timezone)

    is_cadence <- grepl("cadence", names(observations))
    if (any(is_cadence)) {
        if (is.na(sport)) {
            observations[is_cadence] <- NULL
        }
        else {
            if (sport == "running") {
                names(observations)[is_cadence] <- "cadence_running"
            }
            if (sport == "swimming" | is.na(sport)) {
                observations[is_cadence] <- NULL
            }
            if (sport == "cycling") {
                names(observations)[is_cadence] <- "cadence_cycling"
            }
        }
    }

    ## Add missing varibles
    missingVars <- namesToBeUsed[match(namesToBeUsed, names(observations), nomatch = 0) == 0]
    if (nrow(observations) > 0) {
        for (nn in missingVars) {
            observations[[nn]] <- NA
        }
    }

    ## convert speed from speedunit to m/s
    if (speedunit != "m_per_s") {
        speedConversion <- match.fun(paste(speedunit, "m_per_s", sep = "2"))
        observations$speed <- speedConversion(observations$speed)
    }

    ## convert distance from distanceunit to m
    if (distanceunit != "m") {
        distanceConversion <- match.fun(paste(distanceunit, "m", sep = "2"))
        observations$distance <- distanceConversion(observations$distance)
    }

    ## use variable order for trackeRdata
    if (any(names(observations) != allnames$human_names)) {
        observations <- observations[, allnames$human_names]
    }

    attr(observations, "sport") <- sport
    attr(observations, "file") <- file

    return(observations)

}

#' @export
#' @rdname readX
readGPX <- function(file,
                    timezone = "",
                    speedunit = "km_per_h",
                    distanceunit = "km",
                    sport = NULL,
                    ...) {

    ## https://developers.strava.com/docs/uploads/ assuming that
    ## regardless if activity is running or cycling cadence will be
    ## named as cadence. If sport is unknown then we assume that both
    ## cadence_running and cadence_cycling are NA

    doc <- safe_read_xml(file)
    ns <- xml_ns(doc)

    children_names <- function(x,
                               xpath,
                               ns) {
        unique(xml_name(xml_children(xml_find_all(x, xpath, ns))))
    }

    ## Core namespaces
    activity_ns <- names(which(ns == "http://www.topografix.com/GPX/1/1")[1])

    extensions_ns <- c("http://www.garmin.com/xmlschemas/TrackPointExtension/v1",
                       "http://www.garmin.com/xmlschemas/TrackPointExtension/v2",
                       "http://www.topografix.com/GPX/1/1",
                       "http://www.garmin.com/xmlschemas/GpxExtensions/v3")
    extensions_ns <- na.omit(sapply(extensions_ns, function(e) names(which(ns == e)[1])))

    ## Guess sport from data
    if (is.null(sport)) {
        sport <- guess_sport(xml_text(xml_find_first(doc, paste0("//", activity_ns, ":", "name"))))
        if (is.na(sport)) {
            sport <- guess_sport(xml_text(xml_find_first(doc, paste0("//", activity_ns, ":", "type"))))
        }
        ## If not successful, try filename
        if (is.na(sport)) {
            sport <- guess_sport(basename(file))
        }
    }

    ## Trackpoint
    tp_xpath <- paste0("//", activity_ns, ":", "trkpt")
    ch_names <- children_names(doc, tp_xpath, ns)
    if (length(ch_names) == 0) {
        stop("No usable data have been found in", file)
    }
    tp_vars <- data.frame(name = ch_names,
                          ns = activity_ns)

    is_extensions <- tp_vars$name == "extensions"
    if (any(is_extensions)) {
        ## remove position
        tp_vars <- tp_vars[!is_extensions, ]
        for (e in extensions_ns) {
            e_xpath <- paste0("//", e, ":", "TrackPointExtension")
            ## Add any extensions
            ch_nam <- children_names(doc, e_xpath, ns[e])
            if (length(ch_nam)) {
                children <- data.frame(name = ch_nam, ns = e)
                tp_vars <- rbind(tp_vars, children)
            }
        }
    }

    ## Manually add power to tp_vars as it does not come with the standard namespaces in gpx
    tp_vars <- rbind(tp_vars, data.frame(name = "power", ns = "d1"))

    is_time <- tp_vars$name == "time"

    tps <- xml_find_all(doc, tp_xpath, ns[activity_ns])
    ## Double loop to extract obs
    observations <- apply(tp_vars, 1, function(vari) {
        c_xpath <- paste0(".", "//", vari["ns"], ":", vari["name"])
        c_ns <- ns[vari["ns"]]
        sapply(tps, function(x) {
            xml_text(xml_find_first(x, c_xpath, c_ns))
        })
    })

    observations <- as.data.frame(matrix(observations, ncol = nrow(tp_vars)), stringsAsFactors = FALSE)

    names(observations) <- tp_vars$name
    observations[!is_time] <- apply(observations[!is_time], 2, as.numeric)

    ## Add lat and lon
    observations$lat <- as.numeric(xml_attr(tps, "lat", ns[activity_ns]))
    observations$lon <- as.numeric(xml_attr(tps, "lon", ns[activity_ns]))

    ## Compute distance
    dists <- sp::spDists(observations[, c("lon", "lat")], longlat = TRUE, segments = TRUE)
    observations$distance <- if (length(dists) == 1) 0 else cumsum(c(0, dists))

    ## human names
    allnames <- generate_variable_names()
    namesOfInterest <- allnames$gpx_names
    namesToBeUsed <- allnames$human_names
    inds <- match(namesOfInterest, names(observations), nomatch = 0)

    observations <- observations[inds]
    names(observations) <- namesToBeUsed[inds!=0]

    ## coerce time into POSIXct
    observations$time <- gsub("[\t\n]", "", observations$time)
    observations$time <- convertTCXTimes2POSIXct(observations$time, timezone = timezone)

    is_cadence <- grepl("cadence", names(observations))
    if (any(is_cadence)) {
        if (is.na(sport)) {
            observations[is_cadence] <- NULL
        }
        else {
            if (sport == "running") {
                names(observations)[is_cadence] <- "cadence_running"
            }
            if (sport == "swimming" | is.na(sport)) {
                observations[is_cadence] <- NULL
            }
        }
    }

    ## Add missing varibles
    missingVars <- namesToBeUsed[match(namesToBeUsed, names(observations), nomatch = 0) == 0]
    if (nrow(observations) > 0) {
        for (nn in missingVars) {
            observations[[nn]] <- NA
        }
    }

    ## convert speed from speedunit to m/s
    if (speedunit != "m_per_s") {
        speedConversion <- match.fun(paste(speedunit, "m_per_s", sep = "2"))
        observations$speed <- speedConversion(observations$speed)
    }

    ## convert distance from distanceunit to m
    if (distanceunit != "m") {
        distanceConversion <- match.fun(paste(distanceunit, "m", sep = "2"))
        observations$distance <- distanceConversion(observations$distance)
    }

    ## use variable order for trackeRdata
    if (any(names(observations) != allnames$human_names)) {
        observations <- observations[, allnames$human_names]
    }

    attr(observations, "sport") <- sport
    attr(observations, "file") <- file

    return(observations)

}

#' @export
#' @rdname readX
readDB3 <- function(file,
                    timezone = "",
                    table = "gps_data",
                    speedunit = "km_per_h",
                    distanceunit = "km",
                    sport = NULL,
                    ...) {

    if (is.null(sport)) {
        sport <- guess_sport(basename(file))
    }

    db <- RSQLite::dbConnect(RSQLite::SQLite(), file)
    mydf <- RSQLite::dbReadTable(conn = db, name = table)
    RSQLite::dbDisconnect(db)

    ## Test for useable data in container file
    if (!nrow(mydf)) {
        stop("no useable data in input")
    }

    ## prepare names
    allnames <- generate_variable_names()
    namesOfInterest <- allnames$db3_names
    namesToBeUsed <- allnames$human_names

    ## extract the interesting variables
    inds <- match(namesOfInterest, names(mydf), nomatch = 0)
    newdat <- mydf[inds]
    names(newdat) <- namesToBeUsed[inds!=0]

    ## coerce time into POSIXct
    newdat$time <- as.POSIXct(newdat$time*24*60*60, origin = "1899-12-30", tz = timezone)

    is_cadence <- grepl("cadence", names(newdat))
    if (any(is_cadence)) {
        if (is.na(sport)) {
            newdat[is_cadence] <- NULL
        }
        else {
            if (sport == "running") {
                names(newdat)[is_cadence] <- "cadence_running"
            }
            if (sport == "swimming" | is.na(sport)) {
                newdat[is_cadence] <- NULL
            }
        }
    }

    ## add missing variables as NA
    missingVars <- namesToBeUsed[match(namesToBeUsed, names(newdat), nomatch = 0) == 0]
    if (nrow(newdat) > 0) {
        for (nn in missingVars) {
            newdat[[nn]] <- NA
        }
    }

    ## convert speed from speedunit to m/s
    if (speedunit != "m_per_s"){
        speedConversion <- match.fun(paste(speedunit, "m_per_s", sep = "2"))
        newdat$speed <- speedConversion(newdat$speed)
    }

    ## convert distance from distanceunit to m
    if (distanceunit != "m"){
        distanceConversion <- match.fun(paste(distanceunit, "m", sep = "2"))
        newdat$distance <- distanceConversion(newdat$distance)
    }

    ## use variable order for trackeRdata
    if (any(names(newdat) != allnames$human_names))
        newdat <- newdat[, allnames$human_names]

    attr(newdat, "sport") <- sport
    attr(newdat, "file") <- file

    return(newdat)
}



#' @details Reading Golden Cheetah's JSON files is experimental.
#' @export
#' @rdname readX
readJSON <- function(file,
                     timezone = "",
                     speedunit = "km_per_h",
                     distanceunit = "km",
                     sport = NULL,
                     ...) {
    ## get all data
    jslist <- jsonlite::fromJSON(file)$RIDE

    ## starting time
    stime <- strsplit(jslist$STARTTIME, " ")[[1]]
    if (timezone == "") timezone <- stime[3]
    stime <- as.POSIXct(strptime(paste(stime[1:2], collapse = "T"),
                                 format = "%Y/%m/%dT%H:%M:%S"), tz = timezone)

    ## Guess sport from data
    if (is.null(sport)) {
        sport <- guess_sport(jslist$TAGS$Sport)
        ## If not successful, try filename
        if (is.na(sport)) {
            sport <- guess_sport(basename(file))
        }
    }

    ## tracking data
    if (!("SAMPLES" %in% names(jslist))) stop("No tracking data available.")
    mydf <- jslist$SAMPLES

    ## prepare names
    allnames <- generate_variable_names()
    namesOfInterest <- allnames$json_names
    namesToBeUsed <- allnames$human_names

    ## extract the interesting variables
    inds <- match(namesOfInterest, names(mydf), nomatch = 0)
    observations <- mydf[inds]
    names(observations) <- namesToBeUsed[inds!=0]

    ## coerce time into POSIXct
    observations$time <- stime + observations$time

    is_cadence <- grepl("cadence", names(observations))

    if (any(is_cadence)) {
        if (is.na(sport)) {
            observations[is_cadence] <- NULL
        }
        else {
            if (sport == "running") {
                names(observations)[is_cadence] <- "cadence_running"
            }
            if (sport == "swimming" | is.na(sport)) {
                observations[is_cadence] <- NULL
            }
        }
    }

    ## add missing variables as NA
    missingVars <- namesToBeUsed[match(namesToBeUsed, names(observations), nomatch = 0) == 0]
    if (nrow(observations) > 0) {
        for (nn in missingVars) {
            observations[[nn]] <- NA
        }
    }

    ## convert speed from speedunit to m/s
    if (speedunit != "m_per_s"){
        speedConversion <- match.fun(paste(speedunit, "m_per_s", sep = "2"))
        observations$speed <- speedConversion(observations$speed)
    }

    ## convert distance from distanceunit to m
    if (distanceunit != "m"){
        distanceConversion <- match.fun(paste(distanceunit, "m", sep = "2"))
        observations$distance <- distanceConversion(observations$distance)
    }

    ## use variable order for trackeRdata
    if (any(names(observations) != allnames$human_names))
        observations <- observations[, allnames$human_names]

    attr(observations, "sport") <- sport

    attr(observations, "file") <- file

    return(observations)

}


#' Read a GPS container file.
#'
#' @param file The path to a tcx, gpx, json or db3 file. Compressed
#'     versions (gz, bz2, xz, zip) of tcx, gpx, and json files are
#'     directly supported.
#' @param type The type of the GPS container file. Supported so far
#'     are \code{tcx}, \code{gpx}, \code{db3}, and \code{json}.
#' @param table The name of the table in the database if \code{type}
#'     is set to \code{db3}, ignored otherwise.
#' @param from_distances Logical. Should the speeds be calculated from
#'     the distance recordings instead of taken from the speed
#'     recordings directly. Defaults to \code{TRUE} for \code{tcx} and
#'     Golden Cheetah's json files and to \code{FALSE} for \code{db3}
#'     files.
#' @param speedunit Character string indicating the measurement unit
#'     of the speeds in the container file to be converted into meters
#'     per second. Default is \code{m_per_s} when \code{type} is
#'     \code{tcx} and \code{km_per_h} when \code{type} is \code{db3}
#'     or \code{json}. See Details.
#' @param distanceunit Character string indicating the measurement
#'     unit of the distance in the container file to be converted into
#'     meters. Default is \code{m} when \code{type} is \code{tcx} and
#'     \code{km} when \code{type} is \code{db3} or \code{json}. See
#'     Details.
#' @param sport What sport does \code{file} contain data from? Either
#'     \code{'cycling'}, \code{'running'}, \code{'swimming'} or
#'     \code{NULL} (default), in which case the sport is directly
#'     obtained from the \code{\link{readX}} extractors.
#' @inheritParams readX
#' @inheritParams get_resting_periods
#' @inheritParams impute_speeds
#' @inheritParams trackeRdata
#' @inheritParams sanity_checks
#' @details
#'
#' Available options for \code{speedunit} currently are
#' \code{km_per_h}, \code{m_per_s}, \code{mi_per_h},
#' \code{ft_per_min} and \code{ft_per_s}.  Available options for
#' \code{distanceunit} currently are \code{km}, \code{m},
#' \code{mi} and \code{ft}.
#'
#' \code{read_container} try to identify the sport from the data in
#' the container file. If that fails, then an attempt is made to guess
#' the sport from keywords in the filename. If identification is not
#' possible then the \code{file} attribute of the returned object has
#' value \code{NA}.
#'
#' @return An object of class \code{\link{trackeRdata}}.
#' @seealso \code{\link{trackeRdata}}, \code{\link{readTCX}}, \code{\link{readDB3}}, \code{\link{readJSON}}
#'
#' @examples
#' filepath <- system.file("extdata/tcx", "2013-06-08-090442.TCX.gz", package = "trackeR")
#' run <- read_container(filepath, type = "tcx", timezone = "GMT")
#' @export
read_container <- function(file,
                           type = c("tcx", "gpx", "db3", "json", "fit"),
                           table = "gps_data",
                           timezone = "",
                           session_threshold = 2,
                           smooth_elevation_gain = TRUE,
                           correct_distances = FALSE,
                           country = NULL,
                           mask = TRUE,
                           from_distances = NULL,
                           speedunit = NULL,
                           distanceunit = NULL,
                           sport = NULL,
                           lgap = 30,
                           lskip = 5,
                           m = 11,
                           silent = FALSE) {
    ## prepare args
    type <- match.arg(tolower(type), choices = c("tcx", "gpx", "db3", "json", "fit"))
    if (is.null(from_distances)){
        from_distances <- if (type == "db3") FALSE else TRUE
    }
    if (is.null(speedunit)){
        speedunit <- switch(type,
                            "tcx" = "m_per_s",
                            "gpx" = "km_per_h",
                            "db3" = "km_per_h",
                            "json" = "km_per_h",
                            "fit" = "m_per_s")
    }
    if (is.null(distanceunit)) {
        distanceunit <- switch(type,
                               "tcx" = "m",
                               "gpx" = "km",
                               "db3" = "km",
                               "json" = "km",
                               "fit" = "m")
    }

    ## read gps data
    dat <- switch(type,
                  "tcx" = readTCX(file = file, timezone = timezone, speedunit = speedunit,
                                  distanceunit = distanceunit, sport = sport),
                  "gpx" = readGPX(file = file, timezone = timezone, speedunit = speedunit,
                                  distanceunit = distanceunit, sport = sport),
                  "db3" = readDB3(file = file, table = table, timezone = timezone,
                                  speedunit = speedunit, distanceunit = distanceunit),
                  "json" = readJSON(file = file, timezone = timezone, speedunit = speedunit,
                      distanceunit = distanceunit),
                  "fit" = readFIT(file = file, timezone = timezone, speedunit = speedunit,
                      distanceunit = distanceunit, sport = sport)
                  )

    ## make trackeRdata object (with all necessary data handling)
    trackerdat <- trackeRdata(dat, units = NULL, sport = sport,
                              correct_distances = correct_distances,
                              smooth_elevation_gain = smooth_elevation_gain,
                              country = country, mask = mask,
                              session_threshold = session_threshold,
                              from_distances = from_distances,
                              lgap = lgap, lskip = lskip, m = m,
                              silent = silent)

    return(trackerdat)
}

#' Read all supported container files from a supplied directory
#'
#' @param directory The path to the directory.
#' @param aggregate Logical. Aggregate data from different files to
#'     the same session if observations are less then
#'     \code{session_threshold} hours apart? Alternatively, data from
#'     different files is stored in different sessions.
#' @param table The name of the table in the database for db3 files.
#' @param from_distances Logical. Should the speeds be calculated from
#'     the distance recordings instead of taken from the speed
#'     recordings directly. Defaults to \code{TRUE} for tcx and Golden
#'     Cheetah's json files and to \code{FALSE} for db3 files.
#' @param speedunit Character string indicating the measurement unit
#'     of the speeds in the container file to be converted into meters
#'     per second. Default is \code{m_per_s} for tcx files and
#'     \code{km_per_h} for db3 and Golden Cheetah's json files. See
#'     Details.
#' @param distanceunit Character string indicating the measurement
#'     unit of the distance in the container file to be converted into
#'     meters. Default is \code{m} for tcx files and \code{km} for db3
#'     and Golden Cheetah's json files. See Details.
#' @param sport What sport do the files in \code{directory} correspond
#'     to? Either \code{'cycling'}, \code{'running'},
#'     \code{'swimming'} or \code{NULL} (default), in which case an
#'     attempt is made to extract the sport from each file in
#'     \code{directory}.
#' @param parallel Logical. Should reading be carried out in parallel?
#'     If \code{TRUE} reading is performed in parallel using the
#'     backend provided to \code{\link{foreach}}. Default is
#'     \code{FALSE}.
#' @param verbose Logical. Should progress reports be printed?
#' @inheritParams readX
#' @inheritParams get_resting_periods
#' @inheritParams impute_speeds
#' @inheritParams trackeRdata
#' @inheritParams sanity_checks
#' @details
#'
#' Available options for \code{speedunit} currently are
#' \code{km_per_h}, \code{m_per_s}, \code{mi_per_h},
#' \code{ft_per_min} and \code{ft_per_s}.  Available options for
#' \code{distanceunit} currently are \code{km}, \code{m},
#' \code{mi} and \code{ft}.
#'
#' If \code{aggregate = TRUE}, then if \code{sport = NULL} the
#' sport in all sessions is determined by the first file read with
#' a sport specification; else if \code{sport} is one of the other
#' valid options it determines the sport for all sessions.
#'
#' @return An object of class \code{\link{trackeRdata}}.
#' @seealso \code{\link{trackeRdata}}, \code{\link{readTCX}}, \code{\link{readDB3}}, \code{\link{readJSON}}
#'
#' @examples
#' \dontrun{
#' filepath <- system.file("extdata/gpx", package = "trackeR")
#' gpx_files <- read_directory(filepath)
#' }
#'
#' @export
read_directory <- function(directory,
                          aggregate = FALSE, ## aggregate data from all files or keep data from different files in different sessions?
                          table = "gps_data",
                          timezone = "",
                          session_threshold = 2,
                          smooth_elevation_gain = TRUE,
                          correct_distances = FALSE,
                          country = NULL,
                          mask = TRUE,
                          from_distances = NULL,
                          speedunit = list(tcx = "m_per_s", gpx = "km_per_h", db3 = "km_per_h", json = "km_per_h", fit = "m_per_s"),
                          distanceunit = list(tcx = "m", gpx = "km", db3 = "km", json = "km", fit = "m"),
                          sport = NULL,
                          lgap = 30,
                          lskip = 5,
                          m = 11,
                          silent = FALSE,
                          parallel = FALSE,
                          verbose = TRUE) {

    tcxFiles <- list.files(directory, pattern = "tcx", ignore.case = TRUE, full.names = TRUE,
                           no.. = TRUE)
    gpxFiles <- list.files(directory, pattern = "gpx", ignore.case = TRUE, full.names = TRUE,
                           no.. = TRUE)
    db3Files <- list.files(directory, pattern = "db3", ignore.case = TRUE, full.names = TRUE,
                           no.. = TRUE)
    jsonFiles <- list.files(directory, pattern = "json", ignore.case = TRUE, full.names = TRUE,
                            no.. = TRUE)
    fitFiles <- list.files(directory, pattern = "fit", ignore.case = TRUE, full.names = TRUE,
                           no.. = TRUE)
    ltcx <- length(tcxFiles)
    lgpx <- length(gpxFiles)
    ldb3 <- length(db3Files)
    ljson <- length(jsonFiles)
    lfit <- length(fitFiles)
    if ((ltcx == 0) & (ldb3 == 0) & (ljson == 0) & (lgpx == 0) & (lfit == 0)) {
        stop("The supplied directory contains no files with the supported formats.")
    }
    lall <- ltcx + lgpx + ldb3 + ljson + lfit
    allFiles <- c(tcxFiles, gpxFiles, db3Files, jsonFiles, fitFiles)
    fileType <- c(rep("tcx", ltcx), rep("gpx", lgpx), rep("db3", ldb3), rep("json", ljson), rep("fit", lfit))

    allData <- list()
    if (aggregate) {
        read_fun <- function(j) {
            currentType <- fileType[j]
            if (verbose) {
                cat("Reading file", allFiles[j], paste0("(file ", j, " out of ", lall, ")"), "...\n")
            }
            try(do.call(what = paste0("read", toupper(currentType)),
                        args = list(file = allFiles[j],
                                    timezone = timezone,
                                    speedunit = speedunit[[currentType]],
                                    distanceunit = distanceunit[[currentType]], sport=sport)))
        }

        foreach_object <- eval(as.call(c(list(quote(foreach::foreach), j = seq.int(lall)))))
        if (parallel) {
            setup_parallel()
            allData <- foreach::`%dopar%`(foreach_object, read_fun(j))
        }
        else {
            allData <- foreach::`%do%`(foreach_object, read_fun(j))
        }

        if (verbose) {
            cat("Cleaning up...")
        }
        sports <- sapply(allData, attr, which = "sport")
        if (length(unique(sports)) > 1) {
            warning(directory, "has files from multiple sports and aggregate = TRUE. Assumming that all files are ", unique(sports)[1])
        }

        allData <- do.call("rbind", allData[!sapply(allData, inherits, what = "try-error")])
        from_distances <- if (is.null(from_distances)) TRUE else from_distances
        allData <- trackeRdata(allData,
                               session_threshold = session_threshold,
                               correct_distances = correct_distances,
                               country = country,
                               mask = mask,
                               smooth_elevation_gain = smooth_elevation_gain,
                               from_distances = from_distances,
                               sport = sport,
                               lgap = lgap,
                               lskip = lskip,
                               m = m,
                               silent = silent)
        if (verbose) {
            cat("Done\n")
        }
    }
    else {
        read_fun <- function(j) {
            currentType <- fileType[j]
            if (verbose) {
                cat("Reading file", allFiles[j], paste0("(file ", j, " out of ", lall, ")"), "...\n")
            }
            out <- try(read_container(file = allFiles[j],
                                      type = currentType,
                                      table = table,
                                      timezone = timezone,
                                      session_threshold = session_threshold,
                                      correct_distances = correct_distances,
                                      smooth_elevation_gain = smooth_elevation_gain,
                                      country = country,
                                      mask = mask,
                                      from_distances = from_distances,
                                      speedunit = speedunit[[currentType]],
                                      distanceunit = distanceunit[[currentType]],
                                      sport = sport,
                                      lgap = lgap,
                                      lskip = lskip,
                                      m = m,
                                      silent = silent), silent = silent)
            out
        }

        foreach_object <- eval(as.call(c(list(quote(foreach::foreach), j = seq.int(lall)))))
        if (parallel) {
            setup_parallel()
            allData <- foreach::`%dopar%`(foreach_object, read_fun(j))
        }
        else {
            allData <- foreach::`%do%`(foreach_object, read_fun(j))
        }

        if (verbose) {
            cat("Cleaning up...")
        }

        allData <- do.call("c", allData[!sapply(allData, inherits, what = "try-error")])
        if (verbose) {
            cat("Done\n")
        }
    }

    ## clean and return
    allData <- allData[!sapply(allData, is.null)]
    allData <- unique(allData)
    if (aggregate) {
        attr(allData, "file") <- rep(NA, length(allData))
    }
    allData
}

#' @export
#' @rdname readX
readFIT <- function(file,
                    timezone = "",
                    speedunit = "m_per_s",
                    distanceunit = "m",
                    sport = NULL,
                    ...) {

    ## Read FIT file using FITfileR
    fit_data <- FITfileR::readFitFile(file)
    records <- FITfileR::records(fit_data)

    ## Extract relevant variables and rename to match trackeR conventions
    available_cols <- names(records)
    observations <- data.frame(
        time = if ("timestamp" %in% available_cols) records$timestamp else NA,
        latitude = if ("position_lat" %in% available_cols) records$position_lat else NA,
        longitude = if ("position_long" %in% available_cols) records$position_long else NA,
        altitude = if ("altitude" %in% available_cols) records$altitude else NA,
        distance = if ("distance" %in% available_cols) records$distance else NA,
        heart_rate = if ("heart_rate" %in% available_cols) records$heart_rate else NA,
        speed = if ("speed" %in% available_cols) records$speed else NA,
        cadence = if ("cadence" %in% available_cols) records$cadence else NA,
        power = if ("power" %in% available_cols) records$power else NA,
        temperature = if ("temperature" %in% available_cols) records$temperature else NA
    )

    ## Convert timestamps to POSIXct
    observations$time <- as.POSIXct(observations$time, tz = timezone)

    ## Guess sport from data if not provided
    if (is.null(sport)) {
        session_data <- FITfileR::getMessagesByType(fit_data, message_type = "session")
        if (length(session_data) > 0 && "sport" %in% names(session_data)) {
            # Get unique sports from all sessions
            unique_sports <- unique(session_data$sport)
            if (length(unique_sports) == 1) {
                sport <- guess_sport(unique_sports)
            } else {
                # If multiple different sports, use the most frequent one
                sport_counts <- sort(table(session_data$sport), decreasing = TRUE)
                warning("Multiple sports found in file: ",
                       paste(names(sport_counts), " (", sport_counts, " sessions)",
                             collapse = ", "),
                       ". Using most frequent: ", names(sport_counts)[1])
                sport <- guess_sport(names(sport_counts)[1])
            }
        }
        ## If not successful, try filename
        if (is.na(sport)) {
            sport <- guess_sport(basename(file))
        }
    }

    ## Handle cadence based on sport type
    if (!is.null(observations$cadence)) {
        if (is.na(sport)) {
            observations$cadence <- NULL
        }
        else {
            if (sport == "running") {
                names(observations)[names(observations) == "cadence"] <- "cadence_running"
            }
            if (sport == "swimming") {
                observations$cadence <- NULL
            }
            if (sport == "cycling") {
                names(observations)[names(observations) == "cadence"] <- "cadence_cycling"
            }
        }
    }

    ## convert speed from speedunit to m/s if needed
    if (speedunit != "m_per_s") {
        speedConversion <- match.fun(paste(speedunit, "m_per_s", sep = "2"))
        observations$speed <- speedConversion(observations$speed)
    }

    ## convert distance from distanceunit to m if needed
    if (distanceunit != "m") {
        distanceConversion <- match.fun(paste(distanceunit, "m", sep = "2"))
        observations$distance <- distanceConversion(observations$distance)
    }

    ## Add missing variables to match trackeRdata format
    allnames <- generate_variable_names()
    missingVars <- allnames$human_names[match(allnames$human_names, names(observations), nomatch = 0) == 0]
    if (nrow(observations) > 0) {
        for (nn in missingVars) {
            observations[[nn]] <- NA
        }
    }

    ## Ensure correct variable order
    observations <- observations[, allnames$human_names]

    ## Set attributes
    attr(observations, "sport") <- sport
    attr(observations, "file") <- file

    return(observations)
}

#' Read XML file safely handling leading whitespace and compression
#' @param file Path to XML file (can be compressed)
#' @return xml_document object
#' @noRd
safe_read_xml <- function(file) {
    # Read file with gzfile (handles both compressed and uncompressed)
    con <- gzfile(file, "r")
    on.exit(close(con))
    
    # Read content and find first XML content
    lines <- readLines(con, warn = FALSE)
    xml_start <- grep("^\\s*<\\?xml|^\\s*<[a-zA-Z]", lines)[1]
    
    if (is.na(xml_start)) {
        stop("No valid XML content found in file")
    }
    
    # Create temporary file with clean content
    temp_file <- tempfile()
    on.exit(unlink(temp_file), add = TRUE)
    
    # Remove leading whitespace from first XML line and write all lines
    lines[xml_start] <- sub("^\\s+", "", lines[xml_start])
    writeLines(lines[xml_start:length(lines)], temp_file)
    
    # Read the cleaned file
    read_xml(temp_file)
}
