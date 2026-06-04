#' Get a Pretty Version String for an Installed Package
#'
#' Returns a human-readable version string for an installed package, including the Git commit hash
#' if the package was installed from GitLab or GitHub.
#'
#' @param pkgname Character. Name of the installed package.
#'
#' @return Character string with the format \code{"pkgname version x.y.z (Git commit abcdefg)"}
#'   if the package was installed from GitHub or GitLab (and the commit hash is available),
#'   or \code{"pkgname version x.y.z"} otherwise. If the package is not installed, returns a message.
#'
#' @examples
#' get_package_version_string("dplyr")
#' get_package_version_string("yourGitLabPackage")
#'
#' @export
get_package_version_string <- function(pkgname, National = "Nederlands") {
  # Get DESCRIPTION info
  desc <- tryCatch(
    packageDescription(pkgname),
    error = function(e) return(NULL)
  )
  if (is.null(desc)){
    if (National == "Nederlands") {
      return(paste0("Package '", pkgname, "' is niet geinstalleerd."))
    }else{
      return(paste0("Package '", pkgname, "' not installed."))
    }
  }
  if (length(desc)==1){
    if (National == "Nederlands") {
      return(paste0("Package '", pkgname, "' is niet geinstalleerd."))
    }else{
      return(paste0("Package '", pkgname, "' not installed."))
    }
  }


  version <- desc$Version
  # Try to get git info (works for remotes::install_gitlab, install_github, etc.)
  sha <- desc$RemoteSha
  remote <- desc$RemoteHost

  # Compose string
  if (!is.null(sha) && nzchar(sha)) {
    # Optionally show just first 7 chars of SHA (common)
    sha_short <- substr(sha, 1, 7)
      if (National == "Nederlands") {
        return(sprintf("%s versie %s (GitLab commit %s)", pkgname, version, sha_short))
      }else{
        return(sprintf("%s version %s (Git commit %s)", pkgname, version, sha_short))
      }
  } else {
    if (National == "Nederlands") {
      return(sprintf("%s versie %s", pkgname, version))
    }else{
      return(sprintf("%s version %s", pkgname, version))
    }
  }
}
