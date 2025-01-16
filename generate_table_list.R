# library
library(DBI)
library(odbc)
library(dplyr)
library(stringr)

# read in env file
readRenviron("C:/Users/BoydClaire/.Renviron")


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

query <-
    glue::glue("
  SELECT *
  FROM
    information_schema.columns
    ;
")

production_columns <- dbGetQuery(con, query)

query = glue::glue("
    SELECT *
    FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE
    WHERE OBJECTPROPERTY(OBJECT_ID(CONSTRAINT_SCHEMA + '.' + QUOTENAME(CONSTRAINT_NAME)), 'IsPrimaryKey') = 1
")

all_production_primary_keys <- dbGetQuery(con, query)

production_primary_keys = all_production_primary_keys %>%
    filter(!grepl("_MNC",COLUMN_NAME)) %>%
    group_by(TABLE_CATALOG, TABLE_SCHEMA, TABLE_NAME) %>%
    arrange(TABLE_CATALOG, TABLE_SCHEMA, TABLE_NAME, ORDINAL_POSITION) %>%
    summarize(PRIMARY_KEYS = stringr::str_c(COLUMN_NAME, collapse = ", "), .groups = "drop")

production_tables = production_columns %>%
    group_by(TABLE_CATALOG, TABLE_SCHEMA, TABLE_NAME) %>%
    summarise(COLUMNS = n(),
              COLUMN_NAMES = stringr::str_c(COLUMN_NAME, collapse = ", "), .groups = "drop"
    ) %>%
    select(TABLE_CATALOG, TABLE_SCHEMA, TABLE_NAME, COLUMNS, COLUMN_NAMES) %>%
    mutate(first_col = str_extract(COLUMN_NAMES, "^[^,]+"),
           PREFIX = ifelse(grepl("_", first_col),
                           sub("^(.*?)_.*$", "\\1", first_col), NA)) %>%
    left_join(production_primary_keys, by=c("TABLE_CATALOG", "TABLE_SCHEMA", "TABLE_NAME"))

DBI::dbDisconnect(con)


# GET FDW AND IAS TABLES

database = "fdw"
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

fdw_columns <- dbGetQuery(con, query)

fdw_tables = fdw_columns %>%
    mutate(TABLE_CATALOG = "FDW") %>%
    rename(TABLE_SCHEMA = OWNER) %>%
    group_by(TABLE_CATALOG, TABLE_SCHEMA, TABLE_NAME) %>%
    summarise(COLUMNS = n(),
              COLUMN_NAMES = stringr::str_c(COLUMN_NAME, collapse = ", "), .groups = "drop"
    ) %>%
    select(TABLE_CATALOG, TABLE_SCHEMA, TABLE_NAME, COLUMNS, COLUMN_NAMES) %>%
    mutate(first_col = str_extract(COLUMN_NAMES, "^[^,]+"),
           PREFIX = ifelse(grepl("_", first_col) & grepl("VW_CAMA", TABLE_NAME),
                           sub("^(.*?)_.*$", "\\1", first_col), NA),
           PRIMARY_KEYS = NA)

DBI::dbDisconnect(con)

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

ias_tables = ias_columns %>%
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

DBI::dbDisconnect(con)

write.csv(fdw_columns, "tables/fdw_columns.csv")
write.csv(fdw_tables, "tables/fdw_tables.csv")
write.csv(production_columns, "tables/production_columns.csv")
write.csv(production_tables, "tables/production_tables.csv")
write.csv(ias_columns, "tables/ias_columns.csv")
write.csv(ias_tables, "tables/ias_tables.csv")


# BIND ALL TABLES TOGETHER

all_tables = rbind(production_tables, fdw_tables, ias_tables) %>%
    select(-first_col)

write.csv(all_tables, "tables/all_tables.csv", na="")


# START HERE IF ABOVE ISNT RUN


# filter tables to only the ones we really use
all_tables <- read_csv("tables/all_tables.csv") %>%
    select(-1)

table_list <- all_tables %>%
    rename(SCHEMA=TABLE_SCHEMA,
           COLS=COLUMNS,
           DATABASE=TABLE_CATALOG) %>%
    filter(!stringr::str_ends(TABLE_NAME, '_20[0-9]{2}$') &
               !(SCHEMA %in% c("dbo", "COMMON", "internal", "DATA_BACKUP", "DataBackup", "CONV", "NYC_STAGE"))) %>%
    select(DATABASE, SCHEMA, TABLE_NAME, PREFIX, COLS, PRIMARY_KEYS)

write.csv(table_list, "tables/table_list.csv", na="")



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


















