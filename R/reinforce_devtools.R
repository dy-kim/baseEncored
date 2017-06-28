#' @importFrom dplyr mutate case_when filter
#' @importFrom purrr map
#' @export
getUnmetVersionDependency <- function(pkgName) {
  getPackageVersionDependency(pkgName) %>%
    mutate(installed =
             map(name, packageVersion) %>%
             map(as.character) %>%
             unlist()) %>%
    mutate(
      fulfilled = case_when(
        .$compare == ">=" ~ package_version(.$installed) >= package_version(.$version),
        .$compare == "==" ~ package_version(.$installed) == package_version(.$version)
      )
    ) %>%
    filter(!fulfilled)
}

#' @export
getPackageVersionDependency <- function(pkgName, libPath = NULL) {
  pkgsVerDeps <-
    plyr::ldply(.data = find.package(pkgName, lib.loc = libPath),
                .fun = getPackageVersionDependencies)

  pkgsVerDeps[order(pkgsVerDeps$name, pkgsVerDeps$version), ]
}

#' @importFrom devtools as.package parse_deps
#' @importFrom plyr ldply
getPackageVersionDependencies <-
  function(pkg, depType = c("depends", "imports")) {
    depType <- match.arg(
      arg = tolower(depType),
      choices = c("depends", "imports", "suggests"),
      several.ok = TRUE
    )

    pkgObj <- devtools::as.package(pkg)
    depList <- pkgObj[depType]
    pkgVerDeps <- plyr::ldply(.data = depList,
                              .fun = devtools::parse_deps,
                              .id = "type")

    pkgVerDepsNamed <- cbind(data.frame(
      package = as.character(pkgObj$package),
      stringsAsFactors = FALSE
    ),
    pkgVerDeps)

    subset(pkgVerDepsNamed, !is.na(version))
  }

#' @export
ENCORED_PKGS <- c(
  "baseEncored",
  "JediETL",
  "SithETL",
  "MillenniumFalcon",
  "ForceLogger",
  "ForceManager",
  "ForceAnalyzer",
  "ForceLearner",
  "ForceEvent",
  "destroyForce",
  "devtools"
)
