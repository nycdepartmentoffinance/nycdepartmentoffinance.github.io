# library
library(DBI)
library(odbc)
library(dplyr)
library(stringr)

library(R.utils)

# read in env file
readRenviron("C:/Users/BoydClaire/.Renviron")


# functions
get_record_count = function(con, schema, table){
    print(paste0("counting rows in ", table))
    query <- glue::glue("
    SELECT count(*)
      FROM {schema}.{table};
    ")

    count = dbGetQuery(con, query)
    num <- as.numeric(count[1, 1])

    print("done.")
    return(num)
}


# GET PRODUCTION TABLES
database = "production"

con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "SQL Server",
                      Server   = Sys.getenv(paste0(database, "_server")),
                      Database = Sys.getenv(paste0(database, "_database")),
                      UID      = Sys.getenv(paste0(database, "_username")),
                      PWD      = Sys.getenv(paste0(database, "_password")),
                      TrustServerCertificate="yes",
                      Port     = 1433)

# production columns

production_columns <- read_csv("tables/production_columns.csv")%>%
    select(-1, -ORDINAL_POSITION)

manual_data_dictionary <- readxl::read_xlsx("tables/Vision 8 - NYCDataDictionary.xlsx")

query = glue::glue("
    SELECT *
    FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE
    WHERE OBJECTPROPERTY(OBJECT_ID(CONSTRAINT_SCHEMA + '.' + QUOTENAME(CONSTRAINT_NAME)), 'IsPrimaryKey') = 1
")

all_production_primary_keys <- dbGetQuery(con, query)


all_production_primary_keys = all_production_primary_keys %>%
    mutate(PRIMARY_KEY = TRUE)


production_columns_joined <- production_columns  %>%
    left_join(all_production_primary_keys, by=c("TABLE_CATALOG", "TABLE_SCHEMA", "TABLE_NAME", "COLUMN_NAME")) %>%
    left_join(manual_data_dictionary, by=c("TABLE_SCHEMA"="Schema", "TABLE_NAME"="Table", "COLUMN_NAME"="Column")) %>%
    rename(LENGTH = CHARACTER_MAXIMUM_LENGTH,
           PRECISION = NUMERIC_PRECISION,
           DESCRIPTION = `Friendly Name`,
           TYPE = DATA_TYPE)  %>%
    filter((TABLE_SCHEMA %in% c("REAL_PROP")) & !stringr::str_ends(TABLE_NAME, '_20[0-9]{2}$') &
               !grepl("CCV_", TABLE_NAME) & !grepl("_bkup", TABLE_NAME) & !grepl("_changelog", TABLE_NAME)
           & !grepl("knTest01", TABLE_NAME)) %>%
    arrange(TABLE_NAME, DESCRIPTION) %>%
    select(TABLE_NAME, COLUMN_NAME, DESCRIPTION, TYPE, PRIMARY_KEY)


write_csv(production_columns_joined, "tables/production_columns_joined.csv")

DBI::dbDisconnect(con)
