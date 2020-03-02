# Rjira.utils.generateCredentials
#
#' Store Rjira_credentials object to local Environment.
#'
#' The object should contain your JIRA domain, login and pass which will be used in Rjira functions globally.

Rjira.utils.storeJiraCredentials <- function() {

  print("Welcome to Rjira. This function will store your JIRA credentials in your global environement and use them inside Rjira package functions. Please follow the instructions below.")

  # Get JIRA domain

  domain <- ""

  while (nchar(domain) == 0) {
    print("Please enter your JIRA domain, ie. 'https://yourjiradomain.com', and press ENTER. Please be informed that this script does not validate if your input is a domain. Remember to put the protocol and domain extension in your input.")
    domain <- readline()
  }

  # Get JIRA login

  print("Please enter your JIRA login, and press ENTER. If your JIRA installation does not require login please enter NULL.")
  login <- readline()

  if (login == "NULL") {
      login <- NULL
    } else {
      login
  }

  print("Please enter your JIRA password, and press ENTER. If your JIRA installation does not require pasword, please enter NULL.")
  pass <- readline()

  if (pass == "NULL") {
    pass <- NULL
    } else {
    pass
  }

  if (is.null(login)) {
    authenticate <- FALSE
  } else {
    authenticate <- TRUE
  }

  Rjira_jiraCredentials <- list(domain = domain,
                            login = login,
                            pass = pass,
                            authenticate = authenticate)

  assign("Rjira_jiraCredentials", Rjira_jiraCredentials, envir = globalenv())

  return(paste("Rjira_jiraCredentials successfully saved to your global Environment. To edit, simply run the Rjira.utils.generateCredentials function one more time."))

}
