#' DSS Catalog store
#'
#' The `dssrip2` catalog store. Caches the catalogs of DSS files.
#'
#' @param condensed If `TRUE`, create a store that calls the condensed
#'   catalog methods.
#'
#' @details The file store stores a list of `hec.heclib.dss.HecDss`
#'   Java object references named according to the normalized file
#'   path of the originating DSS file. The file store provides four
#'   methods:
#'   - `get()`: Retrieve a DSS catalog from the store.
#'   - `set()`: Create a DSS catalog and add it to the store.
#'   - `drop()`: Remove a DSS catalog from the store.
#'   - `list()`: List the DSS catalog names in the store.
#'  If the `set()` method fails with a message that `javaHeclib.dll`
#'  cannot be found, this usually indicates that `dss.home` is not set
#'  correctly.
#'
#' @importFrom rJava .jevalArray .jcall
#' @keywords internal
.catalog_store = function(condensed) {
  .catalog_list = list()
  .condensed = condensed
  list(
    get = function(filepath) {
      .catalog_list[[filepath]]
    },
    set = function(filepath, rebuild) {
      if (is.null(.catalog_list[[filepath]])) {
        file = dss_file(filepath)
        on.exit(file$done(), add = TRUE)
        if (.condensed) {
          catalog = .jevalArray(file$getCondensedCatalog()$toArray())
        } else {
          catalog = .jevalArray(file$getCatalogedPathnames(rebuild)$toArray())
        }
        .catalog_list[[filepath]] <<- sapply(catalog,
          .jcall, returnSig = "S", "toString")
      }
    },
    drop = function(filepath) {
      .catalog_list[[filepath]] <<- NULL
    },
    list = function() {
      names(.catalog_list)
    }
  )
}
.catalog_condensed = .catalog_store(TRUE)
.catalog_full = .catalog_store(FALSE)


#' Get DSS Paths
#'
#' Get a list of DSS paths in a file.
#'
#' @inheritParams dss_squeeze
#' @param pattern character string containing a regular expression to
#'   be matched against the DSS pathname catalog.
#' @param condensed If `TRUE` returned the condensed path catalog.
#' @param rebuild If `TRUE`, force rebuild the catalog.
#' @return A vector of paths.
#'
#' @inheritSection dss_connect Java Virtual Machine Parameters
#'
#' @seealso [dss_read()] [dss_parts_split()]
#'   [dss_parts_combine()] [dss_parts_replace()]
#'
#' @importFrom rJava .jevalArray
#' @export
dss_catalog = function(filename, pattern = ".*", condensed = TRUE,
  rebuild = FALSE) {
  if (length(pattern) != 1L) {
    stop("Argument \"pattern\" must be length 1")
  }
  assert_dss_connected()
  filename = normalize_path(filename, TRUE)
  if (condensed) {
    paths = dss_catalog_condensed(filename, rebuild)
  } else {
    paths = dss_catalog_full(filename, rebuild)
  }
  as.character(paths)[grep(toupper(pattern), toupper(paths))]
}


#' DSS Catalog Retrieval
#'
#' @param filename The normalized DSS file path.
#' @param rebuild If TRUE, rebuild the catalog. For both condensed and
#'   full catalog stores, the existing catalog will be dropped.
#'   Additionally, for full catalog stores the `rebuild` parameter will
#'   be passed to the `getCatalogedPathnames()` Java method.
#' @return A character vector of pathnames.
#' 
#' @keywords internal
dss_catalog_condensed = function(filename, rebuild) {
  if (rebuild) {
    .catalog_condensed$drop(filename)
  }
  if (!(filename %in% .catalog_condensed$list())) {
    .catalog_condensed$set(filename)
  }
  .catalog_condensed$get(filename)
}


#' @rdname dss_catalog_condensed
#' @keywords internal
dss_catalog_full = function(filename, rebuild) {
  if (rebuild) {
    .catalog_full$drop(filename)
  }
  if (!(filename %in% .catalog_full$list())) {
    .catalog_full$set(filename, rebuild)
  }
  .catalog_full$get(filename)
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
#' @seealso [dss_catalog()] [dss_parts_combine()] [dss_parts_replace()]
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
#' @seealso [dss_catalog()] [dss_parts_split()] [dss_parts_replace()]
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
#' @seealso [dss_catalog()] [dss_parts_split()] [dss_parts_combine()]
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
