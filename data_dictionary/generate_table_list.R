# library
library(DBI)
library(odbc)
library(dplyr)
library(stringr)
library(R.utils)

# logical argument for if to rerun IAS (only twice a year)

args = commandArgs(trailingOnly=TRUE)

if (length(args) == 0){
    rerun_ias = FALSE
} else {
    rerun_ias = as.logical(args[1])
}


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


# GET PRODUCTION COLUMNS/TABLES
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

# get record counts of tables

query <-
    glue::glue("
    SELECT T.*,
        T.NAME AS TABLE_NAME,
        P.[ROWS] AS RECORDS
    FROM SYS.TABLES T
    INNER JOIN  SYS.PARTITIONS P
        ON T.OBJECT_ID=P.OBJECT_ID
    LEFT JOIN sys.all_objects A
        ON T.OBJECT_ID=A.OBJECT_ID;
")

prod_rows <- dbGetQuery(con, query)

query <-
    "
    SELECT *
    FROM sys.schemas
    "

schemas <- dbGetQuery(con, query) %>%
    rename(TABLE_SCHEMA = name)


production_rows <- prod_rows %>%
    left_join(schemas, by="schema_id") %>%
    group_by(object_id) %>%
    slice(1) %>%
    ungroup() %>%
    rename(LAST_UPDATE = modify_date) %>%
    select(TABLE_SCHEMA, TABLE_NAME, RECORDS, LAST_UPDATE)


production_tables = production_columns %>%
    group_by(TABLE_CATALOG, TABLE_SCHEMA, TABLE_NAME) %>%
    summarise(COLUMNS = n(),
              COLUMN_NAMES = stringr::str_c(COLUMN_NAME, collapse = ", "), .groups = "drop"
    ) %>%
    select(TABLE_CATALOG, TABLE_SCHEMA, TABLE_NAME, COLUMNS, COLUMN_NAMES) %>%
    mutate(first_col = str_extract(COLUMN_NAMES, "^[^,]+"),
           PREFIX = ifelse(grepl("_", first_col),
                           sub("^(.*?)_.*$", "\\1", first_col), NA)) %>%
    left_join(production_primary_keys, by=c("TABLE_CATALOG", "TABLE_SCHEMA", "TABLE_NAME")) %>%
    left_join(production_rows,by=c("TABLE_SCHEMA", "TABLE_NAME"))

DBI::dbDisconnect(con)

write.csv(production_columns, "tables/production_columns.csv")
write.csv(production_tables, "tables/production_tables.csv")


# GET FDW TABLES

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

query <-
    glue::glue("
    SELECT *
      FROM FDW3NF.VW_ETL_REFRESH_STATS
      ORDER BY TS_REFRESH DESC
        ;
    ")

stats <- dbGetQuery(con, query)

fdw_refresh <- stats %>%
    filter(DB_NM %in% c("DOFEDPRD"),
        !(PROJECT_NM %in% c("FDW_HIST"))) %>%
    group_by(DB_NM, SCHEMA_NM, TABLE_NM) %>%
    slice(1) %>%
    mutate(FDW_NAME = paste0("VW_", TABLE_NM),
           FDW_LAST_UPDATE = TS_REFRESH) %>%
    ungroup() %>%
    rename(TABLE_CATALOG = DB_NM,
           TABLE_SCHEMA = SCHEMA_NM,
           TABLE_NAME = FDW_NAME) %>%
    select(TABLE_SCHEMA, TABLE_NAME, FDW_LAST_UPDATE)

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
           PRIMARY_KEYS = NA
    ) %>%
    rowwise() %>%
    mutate(RECORDS = get_record_count(con, database_schema, TABLE_NAME))

fdw_tables_joined = fdw_tables %>%
    left_join(fdw_refresh, by=c("TABLE_SCHEMA", "TABLE_NAME"))

DBI::dbDisconnect(con)

write.csv(fdw_columns, "tables/fdw_columns.csv")
write.csv(fdw_tables_joined, "tables/fdw_tables.csv")


# GET IAS TABLES

if(rerun_ias == TRUE){

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
}

ias_columns <- readr::read_csv("tables/ias_columns.csv") %>%
    select(-1)

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

ias_tables_without_records = readr::read_csv("tables/ias_tables_without_records.csv") %>%
    select(-1) %>%
    mutate(RECORDS = NA,
           LAST_UPDATE = NA)


# taking too long - need to come up with other solution

# ias_tables <- ias_tables_without_records %>%
#     rowwise() %>%
#     mutate(RECORDS = ifelse(!grepl("ADJ", TABLE_NAME), get_record_count(con, database_schema, TABLE_NAME), NA),
#            LAST_UPDATE = NA)

DBI::dbDisconnect(con)


write.csv(ias_tables_without_records, "tables/ias_tables.csv")






# BIND ALL TABLES TOGETHER

production_tables = readr::read_csv("tables/production_tables.csv") %>%
    select(-1)

fdw_tables = readr::read_csv("tables/fdw_tables.csv") %>%
    select(-1) %>%
    rename(LAST_UPDATE = FDW_LAST_UPDATE)

ias_tables = readr::read_csv("tables/ias_tables.csv") %>%
    select(-1)


all_tables = rbind(production_tables, fdw_tables, ias_tables) %>%
    select(-first_col)

write.csv(all_tables, "tables/all_tables.csv", na="")


# filter tables to only the ones we really use
all_tables <- read_csv("tables/all_tables.csv") %>%
    select(-1)

table_list <- all_tables %>%
    rename(SCHEMA=TABLE_SCHEMA,
           COLS=COLUMNS,
           DATABASE=TABLE_CATALOG) %>%
    filter(!stringr::str_ends(TABLE_NAME, '_20[0-9]{2}$') &
               !(SCHEMA %in% c("dbo", "COMMON", "internal", "DATA_BACKUP", "DataBackup", "CONV", "NYC_STAGE"))) %>%
    select(DATABASE, SCHEMA, TABLE_NAME, PREFIX, COLS, RECORDS, PRIMARY_KEYS)

write.csv(table_list, "tables/table_list.csv", na="")





















