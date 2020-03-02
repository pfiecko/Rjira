# Rjira.jobs.getAllIssues.r
#
#' High level function to get all issues from your JIRA database for a specific project.
#'
#' Before proceeding please run Rjira.utils.storeJiraCredentials and Rjira.utils.storeOdbcCredentials if you want to save your output to database.
#'
#'@param project - Your JIRA project name your wish to fetch issues from.
#'@param save - TRUE or FALSE whether to save your output to database. Default - TRUE.
#'@return - List of parsed issues from your JIRA project.
Rjira.jobs.getIssues.all <- function(project, save = TRUE) {

  logger.log <- function(message, functionName = NULL) {
    logMessage <- paste(lubridate::now(), " - Rjira - (", functionName, ") - ", message, sep = "")
    return(print(logMessage))
  }

  jobId <- stringi::stri_rand_strings(1, 42)
  assign("jobId", jobId, envir = globalenv())
  logger.log(paste("Starting job:", jobId), "Rjira.jobs.getIssues.all")

  # Prepare control vector for the loop over all issues in project.

  totalIssues <- Rjira.getJQL.project.totalIssues(project)
  controlVec <- seq(from = 0, to = totalIssues, by = 100)

  # Prepare empty tibble of all issues in db.

  curIssuesInDb <- tibble::tibble(id = "00000000", key = "00000000", checksum = "00000000")

  # Loop over.

  for (i in controlVec) {

    logger.log(paste("Fetching page number -", (i + 100)/100, " - of all issues in project:", project), "Rjira.jobs.getIssues.all")

    response <- jira.getJQL.project(i, project, 100)

    pageNamesDict <- response %>%
      Rjira.parseIssues.customFieldsDictionary()

    Rjira.parseIssues(response, pageNamesDict, save = save)

  }

  return(logger.log(paste("Job:", jobId, "completed."), "Rjira.jobs.getIssues.all"))

}

#' High level function to get all issues from your JIRA database for a specific project, updated within a specific timeframe.
#'
#' Before proceeding please run Rjira.utils.storeJiraCredentials and Rjira.utils.storeOdbcCredentials if you want to save your output to database.
#'
#'@param project - Your JIRA project name your wish to fetch issues from.
#'@param update - Code of a time period to determine the update window. 'Xh' - X hours, 'Xd' - X days, 'Xw' - X weeks, 'Xm' - X months. Ie. '5d' will only fetch issues updated within last 5 days.
#'@param save - TRUE or FALSE whether to save your output to database. Default - TRUE.
#'@return - List of parsed issues from your JIRA project.
#'
Rjira.jobs.getIssues.updated <- function(project, update, save = TRUE) {

  logger.log <- function(message, functionName = NULL) {
    logMessage <- paste(lubridate::now(), " - Rjira - (", functionName, ") - ", message, sep = "")
    return(print(logMessage))
  }

  jobId <- stringi::stri_rand_strings(1, 42)
  assign("jobId", jobId, envir = globalenv())
  logger.log(paste("Starting job:", jobId), "Rjira.jobs.getIssues.updated")

  logger.log("Getting number of all issues to be fetched...", "Rjira.jobs.getIssues.updated")
  totalIssuesUpdated <- Rjira.getJQL.project.totalIssues.update(project, update)

  if (totalIssuesUpdated == 0) {

    logger.log(message = paste("Found no updated issues. Job:", jobId, "terminates here."),
               functionName = "Rjira.jobs.getIssues.updated")
    quit()

  } else {

    logger.log(paste(totalIssuesUpdated, "updated issues found. Preparing pagination conrtol vector..."), "Rjira.jobs.getIssues.updated")

    controlVecUpdated <- seq(from = 0, to = totalIssuesUpdated, by = 100)

    logger.log(paste(totalIssuesUpdated, "will be downloaded in", length(controlVecUpdated), "batches of", opt$maxResults, "issues."), "Rjira.jobs.getIssues.updated")

    # Get all issues currently in db with checksum

    curIssuesInDb <- RODBC::sqlQuery(channel = RODBC::odbcConnect(Rjira_odbcCredentials$dns, uid = Rjira_odbcCredentials$user, pwd = Rjira_odbcCredentials$pass, rows_at_time = 1),
                                     query = paste("SELECT [id],[key],[checksum] FROM [", Rjira_odbcCredentials$database,"].[", Rjira_odbcCredentials$schema, "].[", Rjira_odbcCredentials$table, "]", sep = ""))

    # Loop over

    for (i in controlVecUpdated) {

      logger.log(paste("Fetching page", (i + 100)/100, "of", (tail(controlVecUpdated, n = 1) + 100)/100, "of total", totalIssuesUpdated, "issues in project:", project, "in job:", jobId, "."), "Rjira.jobs.allIssues.updated")

      response <- Rjira.getJQL.project.update(i, project, 100, update = update)

      pageNamesDict <- response %>%
        Rjira.parseIssues.customFieldsDictionary()

      Rjira.parseIssues(response, pageNamesDict, save = opt$saveToDb)

    }

    return(
      logger.log(message = paste("Job with id: ", jobId, ". For project: ", opt$project, ", completed successfully.", sep =""),
                 functionName = "Rjira.jobs.getIssues.updated")
    )

  }

}
