# Rjira.utils.storeOdbcCredentials.win.r
#
#' Store Rjira_odbcCredentials object to local Environment.
#'
#' The object should contain your odbc credentials if you want to save Rjira pipeline results to SQL database. These information include on Windows - login, pass and DNS name. These will be used in Rjira functions globally.

Rjira.utils.storeOdbcCredentials.win <- function() {

  print("This function will store your DB connection credentials in your global environement and use them inside Rjira package functions. Please follow the instructions below.")

  # Get JIRA domain

  dns <- ""

  while (nchar(dns) == 0) {
    print("Please enter your DNS name, and press ENTER. Before you proceed - please add your DNS under 'Data sources' in your Windows.")
    dns <- readline()
  }

  # Get JIRA login

  print("Please enter your DNS user name, and press ENTER")
  user <- readline()

  if (user == "NULL") {
    user <- NULL
  } else {
    user
  }

  print("Please enter your DNS password, and press ENTER. If your JIRA installation does not require pasword, please enter NULL.")
  pass <- readline()

  if (pass == "NULL") {
    pass <- NULL
  } else {
    pass
  }

  print("Please enter your DNS main table name, and press ENTER.")
  table <- readline()

  if (table == "NULL") {
    table <- NULL
  } else {
    table
  }

  Rjira_odbcCredentials <- list(dns = dns,
                                user = user,
                                pass = pass,
                                table = table)

  assign("Rjira_odbcCredentials", Rjira_odbcCredentials, envir = globalenv())

  return(paste("Rjira_odbcCredentials successfully saved to your global Environment. To edit, simply run the Rjira.utils.storeOdbcCredentials.win function one more time."))

}
