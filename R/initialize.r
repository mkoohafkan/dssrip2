hecJavaConnectionDB = new.env()


#' DSS Message Level
#'
#' Set the DSS message level.
#'
#' @param message_level An integer specifying the DSS message level.
#'   Default is 2L. Use 1L to suppress all messages from DSS.
#'
#' @seealso [dss_connect()]
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
#' @param monolith If `TRUE`, connect to HEC-Monolith libraries.
#'
#' @details both [dss_connect()] and [dss_require()] configure the JVM
#'   to use the HEC-DSS Java library. [dss_require()] is designed for
#'   use inside other packages; it returns `FALSE` and gives a
#'   warning (rather than an error as [dss_connect()] does)
#'   if the HEC-DSS Java library cannot be loaded correctly.
#'
#' @section Java Virtual Machine Parameters:
#' When working with very large DSS files, you may need to initialize
#' the Java Virtual Machine (JVM) with a larger heap size. For more
#' information see the help vignette:
#' `vignette("Java Virtual Machine Parameters", package = "dssrip2")`
#'
#' @seealso [dss_message_level()] [dss_install_monolith()]
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
  # default is message level 2
  dss_message_level(c(message_level, 2L)[1])
  assign("DSS_CONNECTED", TRUE, hecJavaConnectionDB)
  invisible()
}


#' Connect To DSS (Windows)
#'
#' Connect to DSS in a Windows environment.
#'
#' @inheritParams dss_connect
#'
#' @importFrom rJava javaImport .jaddLibrary .jaddClassPath
#'   .jcall
#' @keywords internal
dss_connect_win = function(dss_home, message_level, monolith) {
  if (is.null(dss_home) && monolith) {
    dss_home = normalizePath(file.path(Sys.getenv("LOCALAPPDATA"),
      "dssrip2", "monolith"), mustWork = FALSE)
  }
  hec_lib_path = normalizePath(file.path(dss_home,
    "lib/javaHeclib.dll"), mustWork = TRUE)

  # initialize JVM
  jar_paths = get_jars(dss_home, monolith)
  javaImport(packages = "java.lang")
  .jaddLibrary("javaHeclib", hec_lib_path)
  .jaddClassPath(jar_paths)
}


#' Connect To DSS (Unix)
#'
#' Connect to DSS in a Unix environment.
#'
#' @inheritParams dss_connect
#'
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
    label = "HEC-Monolith"
    required_jars = c("hecnf.*.jar", "hec-monolith(?!-compat).*.jar",
      "hec-monolith-compat.*.jar", "hec-nucleus-data.*.jar",
      "hec-nucleus-metadata.*.jar", "flogger(?!-system-backend).*.jar",
      "flogger-system-backend.*.jar")
  } else {
    label = "HEC-DSSVue"
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
    warning("Multiple matches found for ", label, " jars:\n",
      paste("\t", sapply(selected_jars[num_found > 1L], paste,
        collapse = ", "), collapse = "\n"), "\n")
    selected_jars = lapply(selected_jars, head, 1L)
  } else if (any(num_found < 1L)) {
    stop("No matches found for ", label, " jars:\n",
      paste("\t", required_jars[num_found < 1L], collapse = "\n"))
  }
  normalizePath(file.path(dss_home, "jar", selected_jars),
    mustWork = TRUE)
}
