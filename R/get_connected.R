#' Define RODBC connection to Oracle
#'
#' @description Creates the `RODBC` connection to Oracle needed to pull SQL queries
#'              from AKFIN database. Also support users who use the R package `keyring`
#'              to store usernames and passwords.
#'
#' @param db String. A registered data source name, in this case `"AKFIN"` by default.
#'        This argument is passed to the `dsn` argument in `RODBC::odbcConnect()`.
#' @param check_access Boolean. If `TRUE` (by default), checks whether you have the
#'        specific tables in AKFIN used in the `crabpack` package. Outputs an error
#'        if the user does not have access to these tables with a message of the
#'        point of contact information for access.
#'
#' @return Channel of class "RODBC". See `?RODBC::odbcConnect()` for more detail.
#'
#' @export
#'


get_connected <- function(db = "AKFIN",
                          check_access = TRUE) {

  # check if database name is stored in keyring, if not request user/pwd
  if(!(db %in% keyring::key_list()[,1])){
    username <- getPass::getPass(msg = paste("Enter your", db,
                                             "Oracle Database Username: "))
    password <- getPass::getPass(msg = paste("Enter your", db,
                                             "Oracle Database Password: "))
  } else{
    username <- keyring::key_list(db)$username
    password <-  keyring::key_get(db, keyring::key_list(db)$username)
  }

  suppressWarnings(channel <- RODBC::odbcConnect(dsn = paste(db),
                                                 uid = paste(username),
                                                 pwd = paste(password),
                                                 believeNRows = FALSE))
  if(channel == -1){
    stop("Unable to connect. Username or password may be incorrect. Please re-enter.\n\n")
    return(invisible())
  }

  if(class(channel) == "RODBC"){
    cat("Successfully connected to Oracle.\n")

    if(check_access & db %in% c("AKFIN", "AFSC")){
      cat("Checking that you have access to the tables queried in the crabpack package.\n")
      tables_to_check <- data.frame(table_name = c("CRABBASE.HAUL",
                                                   "CRABBASE.SPECIMEN",
                                                   "CRABBASE.SIZEGROUPS",
                                                   "CRABBASE.DISTRICT_STRATUM",
                                                   "CRABBASE.STRATUM_STATIONS",
                                                   "CRABBASE.STRATUM_AREA",
                                                   "CRABBASE.STRATUM_DESIGN",
                                                   "CRABBASE.CHIONOECETES_MAT_RATIO",
                                                   "CRABBASE.CHIONOECETES_MATMODEL_PARAMS"),
                                                   access = F)

      for(itable in 1:nrow(x = tables_to_check)){
        table_check <- tryCatch(expr = RODBC::sqlFetch(channel = channel,
                                sqtable = tables_to_check$table_name[itable],
                                max = 5),
                       error = function(cond) data.frame())
        if(nrow(x = table_check) == 5)
          tables_to_check$access[itable] <- TRUE
      }

      if(all(tables_to_check$access == T)){
        cat("Confirming connection to all Oracle tables associated with the crabpack package.\n")
        return(channel)
      } else(
        stop("Cannot connect to these tables in Oracle:\n",
             paste0(tables_to_check$table_name[tables_to_check$access == F],
                    collapse = "\n"),
             "\n\nPlease contact shannon.hennessey@noaa.gov for access to these tables and then try connecting again.")
      )

    } else(return(channel))
  }
}
