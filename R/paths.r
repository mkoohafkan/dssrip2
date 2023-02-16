#' Get DSS Paths
#'
#' Get a list of DSS paths in a file.
#'
#' @inheritParams dss_squeeze
#' @inheritParams base::grep
#' @param condensed If `TRUE` returned the condensed path catalog.
#' @param rebuild If `TRUE`, force rebuild the catalog.
#' @return A vector of paths.
#'
#' @export
dss_catalog = function(file, pattern = "*", condensed = TRUE,
  rebuild = FALSE) {
  assert_dss_connected()
  assert_dss_file(file)
  if (condensed) {
    paths = .javaVectorToStrings(file$getCondensedCatalog())
  } else {
    paths = .javaVectorToStrings(file$getCatalogedPathnames(rebuild))
  }
  paths[grep(pattern, paths)]
}


#' DSS Path To Parts
#'
#' Split DSS path into parts.
#'
#' @param path A vector of DSS paths.
#' @param keep If `TRUE`, preserve the supplied paths in the output.
#' @return A dataframe with fields "A", "B", "C",
#'   "D", "E", "F" and optional field "path" (if `keep = TRUE`).
#'
#' @export
dss_parts_split = function(path, keep = FALSE) {
  assert_path_format(path)
  parts = as.data.frame(do.call(rbind, strsplit(path, "/")))[, -1L]
  # drop first column, will be blank
  names(parts) = DSS_PARTS
  if (keep) {
    cbind(path, parts[DSS_PARTS])
  } else {
    parts[DSS_PARTS]
  }
}


#' DSS Parts To Path
#'
#' Combine DSS parts into a path.
#'
#' @param parts A dataframe of DSS path parts "A", "B", "C",
#'   "D", "E", "F".
#' @return A vector of paths.
#'
#' @export
dss_parts_combine = function(parts) {
  assert_parts_format(parts)
  path = paste0("/", do.call(paste, c(as.list(parts[DSS_PARTS]),
    sep = "/")), "/")
  path
}


#' Replace DSS Path Parts
#'
#' Replace DSS path parts with specified values.
#'
#' @inheritParams dss_parts_split
#' @param replacement A list of replacement path parts "A", "B", "C",
#'   "D", "E", "F".
#' @return A vector of paths, with parts replaced.
#'
#' @export
dss_parts_replace = function(path, replacement = list()) {
  bad = setdiff(names(replacement), DSS_PARTS)
  if (length(bad) > 0L) {
    stop("Unrecognized path parts: ", paste(bad, collapse = ", "))
  }
  parts = dss_parts_split(path)
  for (part in names(replacement)) {
    parts[part] = replacement[[part]]
  }
  dss_parts_combine(parts)
}
