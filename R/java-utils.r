
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
