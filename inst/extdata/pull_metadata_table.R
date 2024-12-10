## Pull Metadata Table from CRABBASE.METADATA_COLUMNS
## Modified from Zack Oyafuso's script in 'gapindex' package


# Restart R Session before running
rm(list = ls())


# Import libraries
library(gapindex)
library(devtools)


# Connect to Oracle and pull CRABBASE.METADATA_COLUMN
sql_channel <- gapindex::get_connected(check_access = FALSE)

metadata_column <- RODBC::sqlQuery(channel = sql_channel,
                                   query = "SELECT METADATA_COLNAME, METADATA_COLNAME_DESC
                                            FROM CRABBASE.METADATA_COLUMN")
names(x = metadata_column) <- c("Field", "Description")


# Write to inst/extdata/ folder
utils::write.csv(x = metadata_column,
                 file = "inst/extdata/metadata.csv",
                 row.names = FALSE)
