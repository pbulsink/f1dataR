#' Load Driver Info
#'
#' @description Loads driver info for all participants in a given season.
#'
#' @param season number from 1950 to current season, or `"current"` (defaults to current season).
#' @importFrom magrittr "%>%"
#' @export
#' @return A tibble with columns `driver_id` (unique and recurring), `given_name`,
#' `family_name`, `nationality`, `date_of_birth` (yyyy-mm-dd format), `code` (driver code), and
#' `permanent_number` (for post-2014 drivers). Returns `NULL` on API failure.
load_drivers <- function(season = get_current_season()) {
  if (season != "current" && (season < 1950 || season > get_current_season())) {
    cli::cli_abort(
      '{.var season} must be between 1950 and {get_current_season()} (or use "current")'
    )
  }

  url <- glue::glue("{season}/drivers.json", season = season)
  data <- get_jolpica_content(url)

  if (is.null(data)) {
    return(NULL)
  }

  data <- data$MRData$DriverTable$Drivers

  data <- add_col_if_absent(data, "code", NA_character_)
  data <- add_col_if_absent(data, "permanentNumber", NA_integer_)

  data %>%
    dplyr::select(
      "driverId",
      "givenName",
      "familyName",
      "nationality",
      "dateOfBirth",
      "code",
      "permanentNumber"
    ) %>%
    tibble::as_tibble() %>%
    janitor::clean_names()
}
