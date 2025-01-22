# library
library(DBI)
library(odbc)
library(dplyr)
library(readr)
library(stringr)

# read in generated table lists

readRenviron("C:/Users/BoydClaire/.Renviron")


production_tables <- readr::read_csv("tables/production_tables.csv") %>%
    select(-1)

fdw_tables <- readr::read_csv("tables/fdw_tables.csv") %>%
    select(-1)

# get time last updated from fdw tables

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
    select(DB_NM, SCHEMA_NM, PROJECT_NM, FDW_NAME, FDW_LAST_UPDATE)


# match production // fdw tables
real_prop = production_tables %>%
    filter(TABLE_SCHEMA == "REAL_PROP")

fdw_crosswalk = fdw_tables %>%
    filter(TABLE_NAME != "CAMA_SALEHIST_HIST") %>%
    mutate(DB = ifelse(grepl("CAMA", TABLE_NAME), "CAMA",
                             ifelse(grepl("VW_PTS_", TABLE_NAME), "PTS",
                                    ifelse(grepl("VW_PDM_", TABLE_NAME), "PDM", NA))),
           CAMA_NAME = sub(".*VW_CAMA_", "", TABLE_NAME),
           HIST_CAMA_NAME = ifelse(grepl("_HIST", TABLE_NAME),
                                   sub("_HIST", "", sub(".*VW_CAMA_", "", TABLE_NAME)), NA)) %>%
    rename(FDW_NAME = TABLE_NAME,
           FDW_CATALOG = TABLE_CATALOG,
           FDW_SCHEMA = TABLE_SCHEMA,
           FDW_COLUMNS = COLUMNS,
           FDW_COLUMN_NAMES = COLUMN_NAMES) %>%
    select(DB, FDW_CATALOG, FDW_SCHEMA, FDW_NAME, CAMA_NAME, HIST_CAMA_NAME, FDW_COLUMNS, FDW_COLUMN_NAMES) %>%
    # EXCEPTIONS FOR JOINING
    mutate(CAMA_NAME = ifelse(CAMA_NAME == "PARCEL_EXTENSIONS", "PARCEL_EXTENSION", CAMA_NAME)) %>%
    left_join(real_prop, by=c("CAMA_NAME"="TABLE_NAME")) %>%
    mutate(CAMA_NAME = ifelse(is.na(TABLE_CATALOG) & is.na(TABLE_SCHEMA), NA, CAMA_NAME),
           IN_CAMA = !is.na(CAMA_NAME)) %>%
    rename(CAMA_COLUMNS = COLUMNS,
           CAMA_COLUMN_NAMES = COLUMN_NAMES,
           CAMA_LAST_UPDATE = LAST_UPDATE) %>%
    left_join(fdw_refresh, by=c("FDW_NAME"))


# how to implement hist tables ?

hist_tables = fdw_crosswalk %>%
    mutate(HIST_CAMA_NAME = ifelse(HIST_CAMA_NAME == "INC_LF_INC_ACTUAL", "INCOME_LF_INC_ACTUAL", HIST_CAMA_NAME)) %>%
    mutate(HIST_CAMA_NAME = ifelse(is.na(HIST_CAMA_NAME) & !is.na(CAMA_NAME), CAMA_NAME, HIST_CAMA_NAME),
           date_key = grepl("DATE_KEY", FDW_COLUMN_NAMES))

duplicates = hist_tables %>%
    filter(!is.na(HIST_CAMA_NAME)) %>%
    group_by(HIST_CAMA_NAME) %>%
    count() %>%
    filter(n > 1)

joining_tables = hist_tables %>%
    filter(HIST_CAMA_NAME %in% duplicates$HIST_CAMA_NAME & !is.na(CAMA_NAME)) %>%
    select(CAMA_NAME, FDW_COLUMNS, PREFIX, PRIMARY_KEYS) %>%
    rename(FDW_COLUMNS_CAMA= FDW_COLUMNS,
           CAMA_PRIMARY_KEYS = PRIMARY_KEYS)

hist_tables_filtered = hist_tables %>%
    filter(HIST_CAMA_NAME %in% duplicates$HIST_CAMA_NAME & is.na(CAMA_NAME)) %>%
    select(FDW_NAME, FDW_LAST_UPDATE, HIST_CAMA_NAME, FDW_COLUMNS, date_key, DB) %>%
    left_join(joining_tables, by=c("HIST_CAMA_NAME"="CAMA_NAME")) %>%
    mutate(one_more_col = (FDW_COLUMNS_CAMA + 1 == FDW_COLUMNS),
           PRIMARY_KEYS = ifelse(date_key & one_more_col & !is.na(CAMA_PRIMARY_KEYS), paste0(CAMA_PRIMARY_KEYS, ", DATE_KEY"), NA),
           CAMA_NAME = NA) %>%
    select(FDW_NAME, FDW_LAST_UPDATE, CAMA_NAME, PREFIX, PRIMARY_KEYS, DB)

fdw_crosswalk_display = fdw_crosswalk %>%
    rows_update(hist_tables_filtered) %>%
    rename(COLS = FDW_COLUMNS) %>%
    # transform date
    mutate(LAST_UPDATE = as.Date(FDW_LAST_UPDATE)) %>%
    select(FDW_NAME, LAST_UPDATE, CAMA_NAME, COLS, PREFIX, PRIMARY_KEYS, DB)

write.csv(fdw_crosswalk_display, "tables/fdw_crosswalk.csv", na="")




