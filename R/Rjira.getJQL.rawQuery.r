# Rjira.getJQL.rawQuery.r
#
#' Get results for JQL query
#'
#' This is the basic, low level request function used by other high level functions in this package. You can use it to get raw results for your query, or use othe high level functions to get specific results for either project or category of issues.
#'
#' @param query - JQL query. For full reference please refer to Atlassian Cloud: https://support.atlassian.com/jira-software-cloud/docs/what-is-advanced-searching-in-jira-cloud/
#' @param responseFormat - format of the API response: parsed / raw / text
#' @param tryAttempts - how many times should JIRA API be querioed if response will not be given immediately. By default 5 attempts are performed
#' @return - Raw JSON response from JIRA API. For further processing it should be parse by Rjira.parseissues function.

Rjira.getJQL.rawQuery <- function(query, responseFormat = "parsed", tryAttempts = 5) {

  # check if Rjira_credentials exist stored in global environement.

  if (!exists("Rjira_credentials", envir = globalenv())) {
    stop()
  }

  # the main request is wrapped in while loop of try attempts.

  attempt <- 1
  request <- "request"
  class(request) <- "try-error"

  while(class(request) == "try-error" && attempt <= tryAttempts) {
    attempt <- attempt + 1
    request <-
      try({

        if (Rjira_credenials$authenticate == TRUE) {

          httr::GET(url = paste(Rjira_credentials$domain, "/rest/api/2/search?jql=", utils::URLencode(query), "&expand=names,changelog", sep = ""),
                    authenticate(Rjira_credentials$login, Rjira_credentials$pass, type = "basic"),
                    add_headers("Content-Type: application/json"))

        } else {

          httr::GET(url = paste(Rjira_credentials$domain, "/rest/api/2/search?jql=", utils::URLencode(query), "&expand=names,changelog", sep = ""),
                    add_headers("Content-Type: application/json"))

        }


      })

  }

  if (class(request) != "try-error") {

    response <- httr::content(request, as = responseFormat)
    return(response)

  } else {

    return(paste("Error fetching response from Jira."))

  }

}
