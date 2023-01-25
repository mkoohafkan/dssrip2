#' Java Version
#'
#' Report the current JVM version.
#'
#' @return The Java version string.
#'
#' @importFrom rJava .jcall
#' @keywords internal
whichJavaVersion = function() {
  .jcall("java/lang/System", "S", "getProperty", "java.runtime.version")
}


#' Java Vector To Strings
#'
#' Convert java vectors returned by catalog functions to strings.
#'
#' @param vect A java vector
#' @return A character vector.
#'
#' @importFrom rJava .jevalArray .jcall
#' @keywords internal
.javaVectorToStrings = function(vect) {
  vectList = .jevalArray(vect$toArray())
  sapply(vectList, .jcall, returnSig = "S", "toString")
}




#' Java Object Fields And Methods
#'
#' Helper functions to get field or call methods of a Java object.
#'
#' @inheritParams build_fields_table
#' @param shortname The Java field name.
#' @return A list of metadata.
#'
#' @importFrom rJava .jfield .jnull
#' @keywords internal
get_field = function(jObject, field) {
  all_fields = get(.jclass(jObject), envir = hecJavaObjectFieldsDB)
  field_def = all_fields[all_fields$SHORTNAME == field, ]

  val = tryCatch(.jfield(jObject, name = field_def$SHORTNAME,
    sig = as.character(field_def$SHORTNAME)), error = function(...) NA,
    silent = TRUE)
  if (identical(.jnull(), val)) {
    NA
  } else {
    val
  }
}


#' @rdname get_field
#' @importFrom rJava .jmethods .jcall
use_method = function(jObject, method, ...) {
  all_methods = get(.jclass(jObject), envir = hecJavaObjectMethodsDB)
  method_def = all_methods[all_methods$SHORTNAME == method, ]
  
  if (nrow(method_def) < 1L) {
    stop("Object ", .jclass(jObject), " does not have method ",
      shQuote(method))
  }

  val = tryCatch(.jcall(jObject, method = method_def$SHORTNAME,
      returnSig = as.character(method_def$SIGNATURE), ...),
    error = function(e) {
      warning(e)
      NA
    }, silent = TRUE)
  if (identical(.jnull(), val)) {
    NA
  } else {
    val
  }
}
