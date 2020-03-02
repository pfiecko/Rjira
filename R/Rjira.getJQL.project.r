# Rjira.getJQL.project.r
#
#' Get JQL query results for specific project.
#'
#' @param startAt - numeric. The index number of results start page to fetch. By default is set to '0', to point at the very 1'st issue.
#' @param project - the name of the project issues should be fetched for.
#' @param maxResults - numeric. The number of issues that should be fetched in a single request. By default is set to '100' which is the maximal number of issues allowed by JIRA API.
#' @param responseFormat - format of the API response: parsed / raw / text. Default is "parsed".

Rjira.getJQL.project <- function(startAt = 0, project, maxResults = 100, responseFormat = "parsed") {

  # check if Rjira_credentials exist stored in global environement.

  if (!exists("Rjira_credentials", envir = globalenv())) {
    stop()
  }

  logger.log <- function(message, functionName = NULL) {
    logMessage <- paste(lubridate::now(), " - Rjira - (", functionName, ") - ", message, sep = "")
    return(print(logMessage))
  }

  query <- paste("project=", project, "&startAt=", startAt, "&maxResults=", maxResults, sep = "")
  logger.log("Request submitted.", "Rjira.getJQL.project")
  output <- Rjira.getJQL.rawQuery(query, responseFormat)
  logger.log("Response received.", "Rjira.getJQL.project")

  return(output)

}

#' Get JQL query results for specific project, updated within given period of time.
#'
#' @param startAt - numeric. The index number of results start page to fetch. By default is set to '0', to point at the very 1'st issue.
#' @param project - the name of the project issues should be fetched for.
#' @param maxResults - numeric. The number of issues that should be fetched in a single request. By default is set to '100' which is the maximal number of issues allowed by JIRA API.
#' @param responseFormat - format of the API response: parsed / raw / text.
#' @param update - code of a time period to determine the update window. 'Xh' - X hours, 'Xd' - X days, 'Xw' - X weeks, 'Xm' - X months. Ie. '5d' will only fetch issues updated within last 5 days.

Rjira.getJQL.project.update <- function(startAt, project, maxResults = 100, update, responseFormat = "parsed") {

  # check if Rjira_credentials exist stored in global environement.

  if (!exists("Rjira_credentials", envir = globalenv())) {
    stop()
  }

  logger.log <- function(message, functionName = NULL) {
    logMessage <- paste(lubridate::now(), " - Rjira - (", functionName, ") - ", message, sep = "")
    return(print(logMessage))
  }

  query <- utils::URLencode(paste("project = ", project, " AND updated >= -", update, "&startAt=", startAt, "&maxResults=", maxResults, sep = ""))
  logger.log("Request submitted.", "Rjira.getJQL.project.update")
  output <- Rjira.getJQL.rawQuery(query, responseFormat)
  logger.log("Response received.", "Rjira.getJQL.project.update")

  return(output)

}

#' Get JQL query results for specific project of the issues of certain type.
#'
#' @param startAt - numeric. The index number of results start page to fetch. By default is set to '0', to point at the very 1'st issue.
#' @param project - the name of the project issues should be fetched for.
#' @param maxResults - numeric. The number of issues that should be fetched in a single request. By default is set to '100' which is the maximal number of issues allowed by JIRA API.
#' @param responseFormat - format of the API response: parsed / raw / text.
#' @param type - one of the values you have in the custom field of 'type' in your JIRA database.

Rjira.getJQL.project.type <- function(startAt = 0, project, type, maxResults = 100, responseFormat = "parsed") {

  # check if Rjira_credentials exist stored in global environement.

  if (!exists("Rjira_credentials", envir = globalenv())) {
    stop()
  }

  logger.log <- function(message, functionName = NULL) {
    logMessage <- paste(lubridate::now(), " - Rjira - (", functionName, ") - ", message, sep = "")
    return(print(logMessage))
  }

  query <- paste("project=", project, "&type=", type, "&startAt=", startAt, "&maxResults=", maxResults, sep = "")
  logger.log("Request submitted.", "Rjira.getJQL.project.type")
  output <- Rjira.getJQL.rawQuery(query, responseFormat)
  logger.log("Response received.", "Rjira.getJQL.project.type")

  return(output)

}
