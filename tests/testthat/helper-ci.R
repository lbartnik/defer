is_travis <- function () identical(tolower(Sys.getenv("TRAVIS")), "true")

is_appveyor <- function () identical(tolower(Sys.getenv("APPVEYOR")), "true")
