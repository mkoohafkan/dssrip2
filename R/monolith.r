#' HEC Monolith Default Directory
#'
#' Platform-dependent default install directory for HEC-Monolith.
#'
#' @keywords internal
monolith_default_dir = function() {
  if (.Platform$OS.type == "windows") {
    normalizePath(
      file.path(Sys.getenv("LOCALAPPDATA"), "dssrip2", "monolith"),
      mustWork = FALSE
    )
  } else {
    normalizePath(
      file.path("~", ".dssrip2", "monolith"),
      mustWork = FALSE
    )
  }
}


#' @rdname monolith_assets
monolith_requirements = function(requirements_file) {
  all_requirements = read_yaml(requirements_file)
  os = tolower(Sys.info()[["sysname"]])
  if (!(os %in% names(all_requirements))) {
    stop(sprintf("No assets listing for platform \"%s\" in %s.",
        os, requirements_file))
  }
  sys_requirements = do.call(c, all_requirements[c("common", os)])
  do.call(rbind, lapply(sys_requirements, as.data.frame))
}


#' Monolith Assets
#'
#' Search for assets listed in a YAML file.
#'
#' @param requirements_file A YAML file listing required assets.
#' @param stop_on_fail If `TRUE`, fail if any assets cannot be found.
#' @return A data frame of required assets.
#'
#' @importFrom yaml read_yaml
#' @keywords internal
monolith_assets = function(requirements_file, stop_on_fail = TRUE) {
  requirements = monolith_requirements(requirements_file)
  requirements_list = split(requirements, requirements[["artifactId"]])
  all_assets = rbind.data.frame(
    monolith_assets = do.call(asset_query_nexus,
      requirements_list[["hec-monolith"]]),
    compat_assets = do.call(asset_query_nexus,
      requirements_list[["hec-monolith-compat"]]),
    data_assets = do.call(asset_query_nexus,
      requirements_list[["hec-nucleus-data"]]),
    meta_assets = do.call(asset_query_nexus,
      requirements_list[["hec-nucleus-metadata"]]),
    hecnf_assets = do.call(asset_query_nexus,
      requirements_list[["hecnf"]]),
    heclib_assets = do.call(asset_query_nexus,
      c(requirements_list[["javaHeclib"]],
        maven.extension = "zip")),
    flogger_assets = do.call(asset_query_maven,
      requirements_list[["flogger"]]),
    flogger_backend_assets = do.call(asset_query_maven,
      requirements_list[["flogger-system-backend"]]),
    make.row.names = FALSE
  )
  assets = merge(requirements, all_assets,
    by = names(requirements), all.x = TRUE)
  missing_assets = is.na(assets[["downloadURL"]])
  if (any(missing_assets)) {
    msg = paste0("Could not find the following assets:\n",
      paste0("\t", paste(assets[["artifactId"]],
          assets[["version"]])[missing_assets], collapse = "\n"))
    if (stop_on_fail) {
      stop(msg)
    } else {
      warning(msg)
    }
  }
  assets
}


#' Maven Helpers
#'
#' Helper functions for searching the Maven central repository.
#'
#' @param id The asset ID.
#' @param extension the Asset file extension.
#' @param The base repository URL.
#'
#' @name maven-helpers
#' @keywords internal
maven_download_url = function(id, extension,
  url = "https://repo.maven.apache.org/maven2") {

  paste(url, maven_path(id, extension), sep = "/")
}


#' @rdname maven-helpers
maven_path = function(id, extension) {
  path_parts = strsplit(id, ":")[[1]]
  path_suffix = paste(gsub("\\.", "/", path_parts[1]), path_parts[2],
    path_parts[3], sep = "/")
  file = paste(paste(path_parts[[2]], path_parts[[3]], sep = "-"),
    extension, sep = ".")
  paste(path_suffix, file, sep = "/")
}


#' @rdname maven-helpers
#' @importFrom httr GET content
maven_sha1 = function(id, extension,
  url = "https://repo.maven.apache.org/maven2") {

  sha_url = paste0(maven_download_url(id, extension, url), ".sha1")
  content(GET(sha_url), "text", encoding = "UTF-8")
}


#' Query Maven Assets
#'
#' Query assets from a Maven repository.
#'
#' @param artifactId The Maven artifact ID.
#' @param repository The Maven repository.
#' @param group The group the asset belongs to.
#' @param maven.extension The asset file extension.
#' @param maven.classifier The asset classifier.
#' @param url The repository base URL.
#' @return A data frame of asset information.
#'
#' @importFrom httr GET content
#' @keywords internal
asset_query_maven = function(artifactId,
  repository = "maven-releases", group = "",
  version = "", maven.extension = "jar", maven.classifier = "",
  url = "https://search.maven.org/solrsearch/select") {

  arg_list = list(
    a = artifactId,
    g  = group,
    v = version,
    p = maven.extension
  )
  arg_list = arg_list[nzchar(arg_list)]
  query = paste0(paste(names(arg_list), arg_list, sep = ":"),
    collapse = "+")
  query_params = list(q = I(query), wt = "json", rows = 1)
  results = GET(url, query = query_params)
  assets = content(results)$response$docs

  data.frame(
    groupId = sapply(assets, function(x) {
      x[["g"]]
    }),
    artifactId = sapply(assets, function(x) {
      x[["a"]]
    }),
    version = sapply(assets, function(x) {
      x[["v"]]
    }),
    id = sapply(assets, function(x) {
      x[["id"]]
    }),
    path = sapply(assets, function(x) {
      maven_path(x[["id"]], x[["p"]])
    }),
    downloadURL = sapply(assets, function(x) {
      maven_download_url(x[["id"]], x[["p"]])
    }),
    checksum.sha1 = sapply(assets, function(x) {
      maven_sha1(x[["id"]], x[["p"]])
    })
  )
}


#' Query Nexus Assets
#'
#' Query assets from a Nexus repository.
#'
#' @inheritParams asset_query_maven
#' @return A data frame of asset information.
#'
#' @importFrom httr GET content
#' @keywords internal
asset_query_nexus = function(artifactId,
  repository = "maven-releases", group = "mil.army.usace.hec",
  version = "", maven.extension = "jar", maven.classifier = "",
  url = "https://www.hec.usace.army.mil/nexus/service/rest/v1/search/assets") {

  query_params = list(
    "repository" = repository,
    "group" = group,
    "version" = version,
    "maven.artifactId" = artifactId,
    "maven.extension" = maven.extension,
    "maven.classifier" = maven.classifier
  )

  results = GET(url, query = query_params)
  assets = content(results)$items
  continuation_token = content(results)$continuationToken
  while (!is.null(continuation_token)) {
    results = GET(url, query = c(query_params,
        list(continuationToken = continuation_token)))
    assets = append(assets, content(results)$items)
    continuation_token = content(results)$continuationToken
  }
  data.frame(
    groupId = sapply(assets, function(x) {
      x[[c("maven2", "groupId")]]
    }),
    artifactId = sapply(assets, function(x) {
      x[[c("maven2", "artifactId")]]
    }),
    version = sapply(assets, function(x) {
      x[[c("maven2", "version")]]
    }),
    id = sapply(assets, function(x) {
      x[["id"]]
    }),
    path = sapply(assets, function(x) {
      x[["path"]]
    }),
    downloadURL = sapply(assets, function(x) {
      x[["downloadUrl"]]
    }),
    checksum.sha1 = sapply(assets, function(x) {
      x[[c("checksum", "sha1")]]
    })
  )
}


#' Install Monolith
#'
#' Download the HEC Monolith Libraries.
#'
#' @param install_path The directory to download the monolith libraries to.
#' @param requirements_file The list of required assets. For expert use only.
#' @param overwrite If `TRUE`, delete any existing data in
#'   `install_path` and recreate.
#'
#' @seealso [dss_connect()]
#'
#' @importFrom utils packageName
#' @importFrom httr GET write_disk
#' @importFrom utils unzip
#' @importFrom digest digest
#' @export
dss_install_monolith = function(install_path, requirements_file,
  overwrite = TRUE) {
  if (missing(install_path)) {
    install_path = monolith_default_dir()
  }
  if (missing(requirements_file)) {
    requirements_file = system.file("requirements.yaml",
      package = packageName())
  }
  assets = monolith_assets(requirements_file)
  lib_path = normalizePath(file.path(install_path, "lib"),
    mustWork = FALSE)
  jar_path = normalizePath(file.path(install_path, "jar"),
    mustWork = FALSE)
  if (dir.exists(install_path) && !overwrite) {
    stop("Directory ", install_path, " already exists.")
  } else {
    unlink(install_path, recursive = TRUE)
    if (!(dir.create(lib_path, recursive = TRUE) &&
          dir.create(jar_path, recursive = TRUE))) {
      stop("Could not create directory ", install_path)
    }
  }

  # download assets
  assets["outputDirectory"] = normalizePath(
    file.path(
      ifelse(grepl(".jar$", assets$downloadURL), jar_path, lib_path),
      basename(assets$downloadURL)
    ),
    mustWork = FALSE
  )
  for (i in seq_len(nrow(assets))) {
    GET(assets[["downloadURL"]][i],
      write_disk(assets[["outputDirectory"]][i], overwrite = FALSE))
    # checksum
    checksum = digest(assets[["outputDirectory"]][i], "sha1", file = TRUE)
    if (!identical(checksum, assets[["checksum.sha1"]][i])) {
      stop("Error downloading ", assets[["outputDirectory"]][i],
        ": SHA1 checksums do not match.")
    }
  }
  # unzip zip files
  zip_assets = assets[grep(".zip$", assets$outputDirectory), ]
  for (zf in zip_assets$outputDirectory) {
    unzip(zf, exdir = dirname(zf))
    unlink(zf)
  }
  invisible()
}
