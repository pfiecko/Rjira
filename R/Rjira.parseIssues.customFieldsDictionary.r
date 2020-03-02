# Rjira.parseIssues.customFieldsDictionary.r
#
#' Build custom fields dictionary from JIRA API response.
#'
#' @param response - The parsed response returned from one of Rjira.getJQL.rawQuery or Rjira.getJQL.project functions.
#' @return A tibble containing dictionary for custom fields mentioned in the JIRA API response.

Rjira.parseIssues.customFieldsDictionary <- function(response) {

  # Helper function to format colum names.

  hlp_formatColumnNames <- function(x) {

    hlp_capitalize <- function(y) {
      c <- strsplit(y, " ")[[1]]
      return(paste(toupper(substring(c, 1,1)), substring(c, 2), sep="", collapse=" "))
    }

    hlp_deCapitalize <- function(y) {
      c <- strsplit(y, " ")[[1]]
      return(paste(tolower(substring(c, 1,1)), substring(c, 2), sep="", collapse=" "))
    }

    stringr::str_split(x, " ")[[1]] %>%
      purrr::map(str_remove_all, "[:punct:]|\\|") %>%
      purrr::map(tolower) %>%
      purrr::map(hlp_capitalize) %>%
      dplyr::unique() %>%
      paste(collapse = "") %>%
      hlp_deCapitalize() %>%
      return()

  }

  logger.log <- function(message, functionName = NULL) {
    logMessage <- paste(lubridate::now(), " - Rjira - (", functionName, ") - ", message, sep = "")
    return(print(logMessage))
  }

  ## field dictionary for page of results

  logger.log("Building dictionary of custom field names...", "Rjira.parseIssues.customFieldsDictionary")

  namesFriendly <- response[["names"]] %>%
    purrr::map(hlp_formatColumnNames)

  namesDict <- tibble::as_tibble(namesFriendly) %>%
    dplyr::pivot_longer(
      everything(),
      names_to = "fieldCode",
      values_to = "fieldFriendlyName") %>%
    dplyr::mutate(fieldFriendlyNameWithId = paste(fieldFriendlyName, stringr::str_remove(fieldCode, "customfield_"), sep = "_"))

  logger.log("Custom field names dictionary built completed.", "Rjira.parseIssues.customFieldsDictionary")

  return(namesDict)

}
