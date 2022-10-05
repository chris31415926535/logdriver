
#' Create a log entry to stdio and/or a Logdriver server
#'
#' To take advantage of asynchronous http calls, make sure to set a future::plan().
#'
#' @param level Log level. Values: "info" (default), "warn", "error", "critical".
#' @param username Character.
#' @param event Character.
#' @param description Character.
#' @param logdriver_appname Mandatory for Logdriver server. Overrides environment variable LOGDRIVER_APPNAME.
#' @param logdriver_host Mandatory for Logdriver server. Overrides environment variable LOGDRIVER_HOST
#' @param logdriver_port Mandatory for Logdriver server. Overrides environment variable LOGDRIVER_PORT
#' @param logdriver_apikey Placeholder for future use.
#'
#' @return Returns a log message, but mainly called for side effects.
#' @export
#'
#' @examples
#' \dontrun{
#' # write log to console
#' add_log(level = "info", username = "chris", event = "created log",
#'         description = "this is a really informative log printed to stdout")
#'
#' # write log to Logdriver server hosted remotely
#' add_log(level = "info", username = "chris", event = "created log",
#'         description = "this log is going on a logdriver server",
#'         logdriver_appname = "testapp", logdriver_host = "logdriver-test.fly.dev",
#'         logdriver_port = "8000" )
#' }
add_log <- function(level = c("info", "warn", "error", "critical"), username = NA, event = NA, description = NA, logdriver_appname = NA, logdriver_host = NA, logdriver_port = NA, logdriver_apikey = NA){

  logdriver_host <- check_env("LOGDRIVER_HOST", logdriver_host, okay_to_have_neither = TRUE)
  logdriver_port <- check_env("LOGDRIVER_PORT", logdriver_port, okay_to_have_neither = TRUE)
  logdriver_appname <- check_env("LOGDRIVER_APPNAME", logdriver_appname, okay_to_have_neither = TRUE)
  logdriver_apikey <- check_env("LOGDRIVER_APIKEY", logdriver_apikey, okay_to_have_neither = TRUE)

  local_only <- FALSE

  # FIXMETODO ensure that appname works okay because we use it to createa sql table later

  # ensure we have values from environment or function parameters
  # we don't handle apikey since not all apps will use one
  if (logdriver_host == "") local_only <- TRUE #stop ("Please set environment variable LOGDRIVER_HOST or provide function parameter logdriver_host.")
  if (!local_only & is.na(logdriver_port)) stop ("Please set environment variable LOGDRIVER_PORT or provide function parameter logdriver_port.")
  if (!local_only & is.na(logdriver_appname)) stop ("Please set environment variable LOGDRIVER_APPNAME or provide function parameter logdriver_appname.")

  level <- match.arg(level, level)

  # get date and time in Greenwich Mean Time / UTC
  # NOTE! this is now done on the server
  #datetime <- as.POSIXlt(Sys.time(), tz = "GMT")

  # format time using sub-second accuracy
  log_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%OS3 %Z")

  if (local_only) {
    logmessage <- sprintf("%s | %s | %s | %s | %s" , log_time, level, username, event, description)
    message(logmessage)
  }


  if (!local_only){
    # set base url

    logdriver_promise <- promises::future_promise(
      seed = NULL,
      expr = {
        base_url <- sprintf("https://%s:%s/append_log", logdriver_host, logdriver_port)

        # create HTTP GET request
        req <- httr2::request(base_url)

        # add parameters
        req <- req %>%
          httr2::req_url_query(
            `appname` = logdriver_appname,
            `level` = level,
            `user` = username,
            `event` = event,
            `description` = description
          )

        resp <- req %>%
          httr2::req_error(is_error = function(resp) FALSE) %>%
          httr2::req_perform()

        logdriver_respcode <- httr2::resp_status(resp)
        logdriver_respbody <- unlist(httr2::resp_body_json(resp))

        if (logdriver_respcode != 200) {
          logmessage <- sprintf("%s | %s | %s | %s | %s ||| %s" , log_time, level, username, event, description, logdriver_respbody)
          message(logmessage)
        }

        if (httr2::resp_status(resp) == 200) {
          logmessage <- logdriver_respbody
          message(logmessage)
        }

        logmessage
      }) %>%
      promises::then(
        onRejected = function(err) {
          logmessage <- sprintf("%s | %s | %s | %s | %s ||| COULD NOT CONNECT TO LOGDRIVER SERVER https://%s:%s", log_time, level, username, event, description, logdriver_host, logdriver_port)
          message(logmessage)
          logmessage
        }
      )

  }


}


# Check to see if we're using environment variable or function variable
# Function variable overrides environment variable
check_env <- function(env_varname, function_var, okay_to_have_neither = FALSE){

  # get the variable names and values for our environment and function parameters
  function_varname <- deparse(substitute(function_var))

  function_val <- function_var

  env_val <- Sys.getenv(env_varname)

  # use environment value by default
  val_to_use <- env_val

  # handle various options
  if (env_val == "" & is.na(function_val) & !okay_to_have_neither) {
    stop(sprintf("Please provide a valid parameter %s or set the environment variable %s.", function_varname, env_varname))
  }

  if (!is.na(function_val))  {
    if (env_val != "") warning(sprintf("Function variable %s='%s' overrides environment parameter %s='%s'.",
                                       function_varname,
                                       function_val,
                                       env_varname,
                                       env_val))
    val_to_use <- function_val
  }

  return(val_to_use)

}
