# Rjira.parseIssues.list.r
#
#' Parse issues on a list returned from Rjira.getJQL.rawQuery or Rjira.getJQL.project.
#'
#' @param response - Parsed response from Rjira.getJQL.project or Rjira.getJQL.project.
#' @param namesDict - The dictionary of custom fields as returned from Rjira.parseIssues.customFieldsDictionary.
#' @param save - TRUE or FALSE on whether to save the results in the database.
#' @return - the list of parsed issues with custom fields with friendly names.

Rjira.parseIssues <- function(response, namesDict, save = TRUE) {

  ### Inner functions

  logger.log <- function(message, functionName = NULL) {
    logMessage <- paste(lubridate::now(), " - Rjira - (", functionName, ") - ", message, sep = "")
    return(print(logMessage))
  }

  # Rename issues custom fields into friendly name format

  issueFieldsRename <- function(issue, namesDict) {

    logger.log(message = paste("Renaming fields for issue: ", issue$key, ".", sep = ""),
                     functionName = "issueFieldsRename@Rjira.parseIssues")

    i <- 1

    for (i in 1:length(issue$fields)) {

      namesDictItem <- namesDict %>%
        dplyr::filter(fieldCode == names(issue$fields[i]))

      names(issue$fields)[i] <- namesDictItem$fieldFriendlyNameWithId

    }

    logger.log(message = paste("Fields for issue: ", issue$key, " successfully renamed.", sep = ""),
               functionName = "issueFieldsRename@Rjira.parseIssues")

    return(issue)

  }

  # Calculate md5 check sum for issue custom fields

  issueMetaChecksum <- function(issue) {

    logger.log(message = paste("Calculating checksum for issue: ", issue$key, "...", sep = ""),
               functionName = "issueMetaChecksum@Rjira.parseIssues")

    return(digest::digest(issue$fields, algo = "md5"))

  }

  # Parse issue changelog

  issueChangelog <- function(issue) {

    parseHistoryItem <- function(historyItem) {

      hiId <- historyItem$id
      hiAuthorName <- historyItem$author$name
      hiAuthorKey <- historyItem$author$key
      hiCreated <- historyItem$created

      hiItems <- historyItem$items %>%
        map_depth(2, hlp_nullToNa) %>%
        map(as_tibble) %>%
        bind_rows()

      hiItems$id <- hiId
      hiItems$authorName <- hiAuthorName
      hiItems$authorKey <- hiAuthorKey
      hiItems$created <- hiCreated

      return(hiItems)

    }

    logger.log(message = paste("Parsing changelog for issue: ", issue$key, "...", sep = ""),
                     functionName = "issueChangelog@Rjira.parseIssues")

    issue$changelog$histories %>%
      purrr::map(parseHistoryItem) %>%
      dplyr::bind_rows() %>%
      dplyr::filter(field == "status") %>%
      return()

  }

  # Parse issue

  parseIssue <- function(issue, namesDict) {

    # add issue meta

    ## issue <- response$issues[[1]]

    logger.log(message = paste("Parsing issue: ", issue$key, "...", sep = ""),
                     functionName = "parseIssue@Rjira.parseIssues")

    logger.log(message = paste("Adding checksum for issue: ", issue$key, "...", sep = ""),
                     functionName = "parseIssue@Rjira.parseIssues")
    issue$checksum <- issueMetaChecksum(issue)

    logger.log.write(message = paste("Adding job id for issue: ", issue$key, "...", sep = ""),
                     functionName = "parseIssue@Rjira.parseIssues")

    # Job id needs to be stored in global env, if not, will be generated here.
    if (exists(jobId)) {
      issue$jobId <- jobId
    } else {
      issue$jobId <- stringi::stri_rand_strings(1, 42)
    }

    logger.log(message = paste("Adding job timestamp for issue: ", issue$key, "...", sep = ""),
                     functionName = "parseIssue@Rjira.parseIssues")
    issue$jobTs <- lubridate::now()

    logger.log(message = paste("Removing meaningless fields for issue: ", issue$key, "...", sep = ""),
                     functionName = "parseIssue@jira.parseIssues")
    issue$expand <- NULL
    issue$self <- NULL

    # rename fields
    logger.log(message = paste("Making friendly names for fields in issue: ", issue$key, "...", sep = ""),
                     functionName = "parseIssue@Rjira.parseIssues")
    issue_fr <- issueFieldsRename(issue, namesDict)

    # parse changelog
    logger.log(message = paste("Parsing changelog for issue: ", issue$key, "...", sep = ""),
                     functionName = "parseIssue@Rjira.parseIssues")
    issue_cl <- issueChangelog(issue)

    logger.log(message = paste("Converting fields and changelog of issue: ", issue$key, " to JSON.", sep = ""),
                     functionName = "parseIssue@Rjira.parseIssues")
    # convert fields lits to text
    issue_fr$fields <- jsonlite::toJSON(issue_fr$fields)
    # add parsed changelog
    issue_fr$changelog <- jsonlite::toJSON(issue_cl)

    # cleanup fields
    issue_fr <- tibble::as_tibble(issue_fr) # %>%
    #  mutate(fields = str_remove_all(fields, regex("\\[|\\]"))) ## %>%
    #  mutate(fields = str_replace_all(fields, "\\[\\[", "\\["),
    #         fields = str_replace_all(fields, "\\]\\]", "\\]"))

    logger.log(message = paste("Parsing of issue: ", issue$key, " - completed.", sep = ""),
                     functionName = "parseIssue@Rjira.parseIssues")

    return(issue_fr)

  }

  saveParsedIssue <- function(parsedIssue) {

    logger.log(message = paste("Saving data for issue: ", parsedIssue$key, " to database on server...", sep = ""),
                     functionName = "saveParsedIssue@Rjira.parseIssues")

    attempt <- 1
    saved <- "saved"
    class(saved) <- "try-error"

    while(class(saved) == "try-error" && attempt <= 5) {

      attempt <- attempt + 1
      saved <- try({
        sqlSave(RODBC::odbcConnect(Rjira_odbcCredentials$DNSname, uid = Rjira_odbcCredentials$user, pwd = Rjira_odbcCredentials$pass, rows_at_time = 1),
                as_tibble(parsedIssue),
                tablename = Rjira_odbcCredentials$table,
                append = TRUE,
                rownames = FALSE,
                fast = FALSE,
                # verbose = TRUE,
                varTypes = c(id = "varchar(MAX)",
                             key = "varchar(MAX)",
                             fields = "text",
                             changelog = "text",
                             checksum = "varchar(MAX)",
                             jobId = "varchar(MAX)",
                             jobTs = "datetime"))
      })

    }

    logger.log(message = paste("Data for issue: ", parsedIssue$key, "successfully saved to database on server.", sep = ""),
                     functionName = "saveParsedIssue@Rjira.parseIssues")

    return()

  }

  checkIfIssueUpdated <- function(parsedIssue) {

    # curIssuesInDb should be stored in global environment

    if (parsedIssue$checksum %in% curIssuesInDb$checksum) {
      logger.log(message = paste("Checksum for issue:", parsedIssue$key, "thesame as in db. Skipping."),
                 functionName = "checkIfIssueUpdated@Rjira.parseIssues")
      return(NULL)
    } else {
      logger.log(message = paste("Checksum for issue:", parsedIssue$key, "different as in db. New version of issue will be saved."),
                       functionName = "checkIfIssueUpdated@Rjira.parseIssues")
      return(parsedIssue)
    }

  }

  ### Main function body

  logger.log(message = "Processing issues in response...",
                   functionName = "Rjira.parseIssues")

  issues <- response[["issues"]]

  pageIssues <- response$issues %>%
    purrr::map(parseIssue) %>%
    purrr::map(checkIfIssueUpdated)

  if (save == T) {

    logger.log(message = "Parsed issues will be saved to database on server.",
                     functionName = "Rjira.parseIssues")

    pageIssues %>%
      purrr::map(saveParsedIssue)

    return("Issues should be saved now in DB.")

  } else {

    logger.log(message = "Parsed issues will be returned as data frame.",
                     functionName = "Rjira.parseIssues")

    return(pageIssues)

  }

}
