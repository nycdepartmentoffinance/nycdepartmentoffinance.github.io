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


# GET IAS TABLES

database = "ias"
database_schema = toupper(Sys.getenv(paste0(database, "_schema")))

# create database connection, by reading in the correct env variables
con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "Oracle in OraClient19Home1",
                      DBQ   = Sys.getenv(paste0(database, "_path")),
                      DATABASE = Sys.getenv(paste0(database, "_schema")),
                      UID      = Sys.getenv(paste0(database, "_username")),
                      PWD      = Sys.getenv(paste0(database, "_password")),
                      TrustServerCertificate="no",
                      Port     = 1433)

query <-
    glue::glue("
    SELECT *
      FROM ALL_TAB_COLUMNS
      WHERE OWNER = '{toupper(Sys.getenv(paste0(database, '_schema')))}'
        ;
    ")

ias_columns <- dbGetQuery(con, query)
write.csv(ias_columns, "tables/ias_columns.csv")

ias_tables_without_records = ias_columns %>%
    mutate(TABLE_CATALOG = "IAS") %>%
    rename(TABLE_SCHEMA = OWNER) %>%
    group_by(TABLE_CATALOG, TABLE_SCHEMA, TABLE_NAME) %>%
    summarise(COLUMNS = n(),
              COLUMN_NAMES = stringr::str_c(COLUMN_NAME, collapse = ", "), .groups = "drop"
    ) %>%
    select(TABLE_CATALOG, TABLE_SCHEMA, TABLE_NAME, COLUMNS, COLUMN_NAMES) %>%
    mutate(PREFIX = NA,
           first_col = NA,
           PRIMARY_KEYS = NA)

write.csv(ias_tables_without_records, "tables/ias_tables_without_records.csv")

# START HERE

ias_tables_without_records = readr::read_csv("tables/ias_tables_without_records.csv") %>%
    select(-1)


ias_tables <- ias_tables_without_records %>%
    rowwise() %>%
    mutate(RECORDS = get_record_count(con, database_schema, TABLE_NAME),
           LAST_UPDATE = NA)

DBI::dbDisconnect(con)


write.csv(ias_tables, "tables/ias_tables.csv")
















