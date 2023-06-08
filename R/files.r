#' DSS File Store
#'
#' The `dssrip2` file store. manages the Java DSS File objects.
#'
#' @details The file store stores a list of `hec.heclib.dss.HecDss`
#'   Java object references named according to the normalized file
#'   path of the originating DSS file. The file store provides four
#'   methods:
#'   - `get()`: Retrieve a DSS file handle from the store.
#'   - `set()`: Create a DSS file handle and add it to the store.
#'   - `drop()`: Remove a DSS file handle from the store.
#'   - `list()`: List the DSS file handle names in the store.
#'  If the `set()` method fails with a message that `javaHeclib.dll`
#'  cannot be found, this usually indicates that `dss.home` is not set
#'  correctly.
#'
#' @importFrom rJava .jcall
#' @importFrom lubridate now
#' @keywords internal
.file_store <- function() {
  .file_list <- list()
  .catalog_list <- list()
  list(
    handle = function(filepath, ...) {
      if (is.null(.file_list[[filepath]])) {
        # create file handle if it doesn't already exist
        .file_list[[filepath]] <<- .jcall("hec/heclib/dss/HecDss",
          "Lhec/heclib/dss/HecDss;", method = "open", filepath, ...)
        .file_list[[filepath]]$done()
        attr(.file_list[[filepath]], "cachetime") <<- now("UTC")
        # initialize catalog
        .catalog_list[[filepath]] <<- list()
      }
      .file_list[[filepath]]
    },
    drop = function(filepath) {
      .file_list[[filepath]]$close()
      .file_list[[filepath]] <<- NULL
      .catalog_list[[filepath]] <<- NULL
    },
    list = function() {
      names(.file_list)
    },
    catalog = function(filepath, condensed, rebuild) {
      if (condensed) {
        type = "condensed"
      } else {
        type = "full"
      }
      if (rebuild) {
        .catalog_list[[c(filepath, type)]] <<- NULL
      }
      if (is.null(.catalog_list[[c(filepath, type)]])) {
        if (type == "condensed") {
            paths = .jevalArray(.file_list[[filepath]]$getCondensedCatalog()$toArray())
        } else {
            paths = .jevalArray(.file_list[[filepath]]$getCatalogedPathnames(rebuild)$toArray())
        }
        .file_list[[filepath]]$done()
        .catalog_list[[c(filepath, type)]] <<- sapply(paths, .jcall,
          returnSig = "S", "toString")
        attr(.catalog_list[[c(filepath, type)]], "cachetime") <<- now("UTC")
      }
      .catalog_list[[c(filepath, type)]]
    },
    list_catalog = function(condensed) {
      if (condensed) {
        type = "condensed"
      } else {
        type = "full"
      }
      names(.catalog_list)[vapply(.catalog_list, function(x) type %in% names(x), TRUE)]
    },
    touch = function(filepath) {
      attr(.file_list[[filepath]], "cachetime") <<- now("UTC")
    }
  )
}
.store <- .file_store()


#' Normalize DSS File Path
#'
#' Normalize a DSS file path. A wrapper for [base::normalizePath()]
#'   with winslash set to `"/"`.
#'
#' @param filename A DSS file path.
#' @param exists If `TRUE`, assert that the DSS file exists.
#' @return A normalized file path.
#'
#' @details This function attempts to strictly resolve file paths
#'   by traversing the file path back to the deepest existing
#'   directory, in order to resolve potential issues with directory
#'   names (e.g., differences in letter case).
#'
#' @keywords internal
normalize_path = function(filename, exists) {
  sep = "/"
  if (exists || file.exists(filename)) {
    normalizePath(filename, winslash = sep, mustWork = TRUE)
  } else if (dirname(filename) == filename) {
    file.path(filename)
  } else {
    normalizePath(file.path(Recall(dirname(filename), exists),
      basename(filename), fsep = sep), winslash = sep,
      mustWork = exists)
  }
}


#' DSS File Handle
#'
#' Retrieve a DSS file handle.
#'
#' @inheritParams normalize_path
#' @param ... Additional arguments passed to the
#'   `hec.heclib.dss.HecDss` Java object `open` method. Typically,
#'   this is an unnamed argument specifying the DSS file version.
#' @return A `hec.heclib.dss.HecDss` Java object reference.
#'
#' @keywords internal
dss_file = function(filename, exists = TRUE, ...) {
  filename = normalize_path(filename, exists)
  .store$handle(filename, ...)
}


#' Create Empty DSS File.
#'
#' Create an empty DSS file.
#'
#' @inheritParams dss_file
#' @param version The DSS version of the created file (6 or 7).
#'
#' @seealso [dss_convert()]
#'
#' @export
dss_create = function(filename, version = 7L) {
  assert_dss_connected()

  version = as.integer(version[1])

  if (!any(version == c(6L, 7L))) {
    stop("Only DSS version 6 or 7 is supported")
  }
  if (file.exists(filename)) {
    stop("File ", filename, " already exists.")
  } 
  if (!dir.exists(dirname(filename))) {
    stop("Directory ", dirname(filename), "does not exist.")
  }
  dss_file(filename, FALSE, version)$done()
  invisible(TRUE)
}


#' Squeeze a DSS File
#'
#' Calls the squeeze method on file to remove deleted or overwritten
#' data.
#'
#' @inheritParams dss_file
#'
#' @seealso [dss_delete()]
#'
#' @export
dss_squeeze = function(filename) {
  assert_dss_connected()
  file = dss_file(filename)
  on.exit(file$done(), add = TRUE)
  file$getDataManager()$squeeze()
  invisible(TRUE)
}


#' DSS File Version
#'
#' Get the DSS file version.
#'
#' @inheritParams dss_file
#' @return An integer
#'
#' @seealso [dss_convert()]
#'
#' @export
dss_version = function(filename) {
  assert_dss_connected()
  file = dss_file(filename)
  on.exit(file$done(), add = TRUE)
  as.integer(file$getDataManager()$getDssFileVersion())
}


#' DSS File Conversion
#'
#' Convert between DSS file versions.
#'
#' @inheritParams dss_file
#' @param to The destination file path.
#'
#' @seealso [dss_version()] [dss_create()]
#'
#' @export
dss_convert = function(filename, to) {
  assert_dss_connected()
  file = dss_file(filename)
  on.exit(file$done(), add = TRUE)
  file$getDataManager()$convertVersion(to)
  invisible(TRUE)
}


#' Close DSS File
#'
#' Close a DSS file. This should only be used when the user has
#' finished working with a DSS file, and is not strictly required
#' as the package will implictly close a DSS file after every
#' operation.
#'
#' @inheritParams dss_file
#'
#' @export
dss_close = function(filename) {
  filename = normalize_path(filename, exists = TRUE)
  if ((filename %in% .store$list())) {
    .store$drop(filename)
  } else {
    warning("File ", filename, " is already closed.")
  }
  invisible(TRUE)
}

#' @rdname dss_close
#'
#' @export
dss_close_all = function() {
  all_files = .store$list()
  lapply(all_files, .store$drop)
  invisible(TRUE)
}
