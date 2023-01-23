hecJavaObjectsDB = new.env()
hecJavaConstantsDB = new.env()


#' DSS Message Level
#'
#' Set the DSS message level.
#'
#' @param message_level An integer specifying the DSS message level.
#'   Default is 2L. Use 1L to suppress all messages from DSS.
#'
#' @importFrom rJava .jcall
#' @export
dss_message_level = function(message_level) {
  # set message level
  tryCatch(.jcall("hec/heclib/util/Heclib", returnSig = "V",
    method = "zset", "MLEVEL", " ", as.integer(message_level)),
    error = function(e) stop("Message level could not be set. ",
      "Check that DSS connection is working.", call. = FALSE))
}


#' @rdname dss_connect
#' @export
dss_require = function(dss_home = getOption("dss.home"),
  message_level = getOption("dss.messagelevel"), monolith = TRUE) {
  result = try(dss_connect(dss_home))
  if (inherits(result, "try-error")) {
    invisible(FALSE)
  } else {
    invisible(TRUE)
  }
}


#' Connect To DSS
#'
#' Connect to DSS via the JVM.
#'
#' @param dss_home The path to the HEC-DSS install folder.
#' @inheritParams dss_message_level
#'
#' @details both [dss_connect()] and [dss_require()] configure the JVM
#'   to use the HEC-DSS Java library. [dss_require()] is designed for
#'   use inside other functions; it returns `FALSE`` and gives a
#'   warning (rather than an error as `[dss_connect()] does by default)
#'   if the HEC-DSS Java library cannot be loaded correctly.
#'
#' @importFrom rJava .jnew J
#' @export
dss_connect = function(dss_home = getOption("dss.home"),
  message_level = getOption("dss.messagelevel"),
  monolith = getOption("dss.monolith")) {
  if (.Platform$OS.type == "windows") {
    dss_connect_win(dss_home, message_level, isTRUE(monolith))
  } else {
    dss_connect_unix(dss_home, message_level, isTRUE(monolith))
  }
  assign("hec.io.TimeSeriesContainer",
    java_fields(.jnew("hec/io/TimeSeriesContainer")), hecJavaObjectsDB)
  assign("hec.io.PairedDataContainer",
    java_fields(.jnew("hec/io/PairedDataContainer")), hecJavaObjectsDB)
  assign("hec.io.GridContainer",
    java_fields(.jnew("hec/io/GridContainer")), hecJavaObjectsDB)
  assign("DSS_CONSTANTS", J("hec/script/Constants"),
    envir = hecJavaConstantsDB)
  # default is message level 2
  dss_message_level(c(message_level, 2L)[1])
  invisible()
}


#' @rdname dss_connect
#' @importFrom rJava javaImport .jaddLibrary .jaddClassPath
#'   .jcall
dss_connect_win = function(dss_home, message_level, monolith) {

  hec_lib_path = normalizePath(file.path(dss_home,
    "lib/javaHeclib.dll"), mustWork = TRUE)

  # initialize JVM
  jar_paths = get_jars(dss_home, monolith)
  javaImport(packages = "java.lang")
  .jaddLibrary("javaHeclib", hec_lib_path)
  .jaddClassPath(jar_paths)
}


#' @rdname dss_connect
#' @keywords internal
dss_connect_unix = function(dss_home, message_level, monolith) {
  stop("Linux/MacOS is not currently supported.")
}


#' Get DSS Jars
#'
#' Get paths to required DSS jars.
#'
#' @param dss_home The path to the DSS install.
#' @return A list of paths to required jars.
#'
#' @importFrom utils head
#' @keywords internal
get_jars = function(dss_home, monolith) {
  if (monolith) {
    required_jars = c("hec-monolith(?!-compat).*.jar", "hecnf.*.jar",
      "hec-monolith-compat.*.jar", "hec-nucleus-metadata.*.jar",
      "flogger(?!-system-backend).*.jar", "flogger-system-backend.*.jar")
  } else {
    required_jars = c("hec.jar", "rma.jar", "lookup.jar",
      "hec-dssvue.+.jar", "help/dssvueHelp.jar")
  }
  all_jars = dir(normalizePath(file.path(dss_home, "jar"),
    mustWork = TRUE), pattern = ".jar$", recursive = TRUE)
  # search for each jar individually so problems can be identified
  jar_indexes = lapply(sprintf("(%s)", required_jars), grep,
    all_jars, perl = T)
  selected_jars = lapply(jar_indexes, function(x)
    all_jars[x])
  num_found = sapply(selected_jars, length)
  if (any(num_found > 1L)) {
    warning("Multiple matching jars found:\n",
      paste("\t", sapply(selected_jars[num_found > 1L], paste,
        collapse = ", "), collapse = "\n"), "\n")
    selected_jars = lapply(selected_jars, head, 1L)
  } else if (any(num_found < 1L)) {
    stop("No matching jars found:\n",
      paste("\t", required_jars[num_found < 1L], collapse = "\n"))
  }
  normalizePath(file.path(dss_home, "jar", selected_jars),
    mustWork = TRUE)
}


#' Fields Data Frame
#'
#' Get a drataframe of Java object fields.
#'
#' @param jObject A Java object.
#' @return A dataframe with fields "FULLNAME", "SHORTNAME", "CLASS",
#'   and "SIGNATURE".
#'
#' @importFrom utils head tail
#' @importFrom rJava .jfields
#' @keywords internal
java_fields = function(jObject) {

  fields = .jfields(jObject)
  parts = strsplit(trimws(gsub("(public)|(private)|(static)|(final)",
    "", fields)), " ", fixed = TRUE)
  objname = sapply(parts, tail, 1L)
  jclass = sapply(parts, head, 1L)
  shortname = sapply(strsplit(objname, ".", fixed = TRUE), tail, 1L)
  signature = gsub("(\\[\\])", "", jclass)

  sigmatches = match(signature, names(sigConversions))
  sigcodes = unlist(ifelse(is.na(sigmatches),
    paste0("L", gsub("\\.", "/", signature), ";"),
    paste0(ifelse(grepl("\\[\\]", jclass), "[", ""),
      sigConversions[sigmatches])))

  data.frame(FULLNAME = fields, SHORTNAME = shortname,
    CLASS = jclass, SIGNATURE = sigcodes)
}


#' Get Metadata Field
#'
#' Helper function to get metadata field value.
#'
#' @inheritParams java_fields
#' @param shortname The Java object shortname.
#' @param signature The Java object signature.
#' @return A list of metadata.
#'
#' @importFrom rJava .jfield .jnull
#' @keywords internal
metafield = function(jObject, shortname, signature) {
  val = tryCatch(.jfield(jObject, name = shortname,
    sig = as.character(signature)), error = function(...) NA,
    silent = TRUE)
  if (identical(.jnull(), val)) {
    NA
  } else {
    val
  }
}


#' Get Java Metadata
#'
#' Get a list of fields in the java object and their values.
#'
#' @inheritParams java_fields
#' @return A named list of metadata.
#'
#' @importFrom rJava .jclass
#' @export
java_metadata = function(jObject) {
  EXCLUDE_FROM_METADATA = c("values", "times", "modified", "quality",
    "xOrdinates", "yOrdinates", "xData", "yData")
  dcClass = .jclass(jObject)
  metaDF = get(dcClass, envir = hecJavaObjectsDB)
  vals = setNames(apply(metaDF, 1, function(x)
      metafield(jObject, x[["SHORTNAME"]], x[["SIGNATURE"]])),
    metaDF[["SHORTNAME"]])
  vals[setdiff(names(vals), EXCLUDE_FROM_METADATA)]
}
