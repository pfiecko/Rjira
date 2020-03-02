# Rjira.getNoOfTotalIssuesInProject.r
#
#' Get the length of the whole issues list in given project.
#'
#' @param project - the name of the project the list of issues should be fetched for.
#' @param responseFormat - format of the API response: parsed / raw / text
#' @return The number of total issues in project.
#'
Rjira.getJQL.project.totalIssues <- function(project, responseFormat = "parsed") {

  # check if Rjira_credentials exist stored in global environement.

  if (!exists("Rjira_credentials", envir = globalenv())) {
    stop()
  }

  query <- paste("project=", project, "&startAt=0&maxResults=0", sep = "")
  response <- Rjira.getJQL.rawQuery(query, responseFormat)
  output <- response[["total"]]

  return(output)

}

#' Get the length of the issues list in given project updated within the given period of time.
#'
#' @param project - The name of the project the list of issues should be fetched for.
#' @param update - Code of a time period to determine the update window. 'Xh' - X hours, 'Xd' - X days, 'Xw' - X weeks, 'Xm' - X months. Ie. '5d' will only fetch issues updated within last 5 days.
#' @return - Number of total issues within a project that were updated within the given period of time.
#'
Rjira.getJQL.project.totalIssues.update <- function(project, update, responseFormat = "parsed") {

  query <- URLencode(paste("project = ", project, " AND updated >= -", update, sep = ""))
  response <- Rjira.getJQL.rawQuery(query, responseFormat)
  output <- response[["total"]]

  return(output)

}
