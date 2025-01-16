# library
library(DBI)
library(odbc)
library(dplyr)
library(stringr)
library(readr)


fdw_columns <- readr::read_csv("tables/fdw_columns.csv") %>%
    select(-1)


pts_tables_only <- fdw_columns %>%
    filter(grepl("VW_PTS", TABLE_NAME) & !(grepl("_HIST", TABLE_NAME))) %>%
    group_by(TABLE_NAME) %>%
    summarize(COLS = n()) %>%
    mutate(PTS_NAME = sub("VW_PTS_*", "", TABLE_NAME))


fdw_pts_tables <- pts_tables_only$TABLE_NAME

# read in all user field descriptions for the 55 user tables

pardat = readxl::read_excel("tables/pts_userfields.xlsx", sheet = "PARDAT")
asmt = readxl::read_excel("tables/pts_userfields.xlsx", sheet = "ASMT")
aprval = readxl::read_excel("tables/pts_userfields.xlsx", sheet = "APRVAL")
usrfield = readxl::read_excel("tables/pts_userfields.xlsx", sheet = "USRFIELD") %>%
    select(-TROWID)


combined_table <- rbind(pardat, asmt, aprval, usrfield) %>%
    rename(COLUMN_NAME = FLD,
           DESCRIPTION = FLDLABL) %>%
    mutate(TABLE_NAME = paste0("VW_PTS_", TBLE)) %>%
    select(TABLE_NAME, COLUMN_NAME, DESCRIPTION)

# ground truth of new page - what we want descriptions for
pts_columns <- fdw_columns %>%
    filter(TABLE_NAME %in% fdw_pts_tables) %>%
    rename(TYPE = DATA_TYPE) %>%
    select(OWNER, TABLE_NAME, COLUMN_NAME, TYPE) %>%
    left_join(combined_table, by=c("TABLE_NAME", "COLUMN_NAME"))

remaining_userfields <- pts_columns %>%
    filter(grepl("USER", COLUMN_NAME) | grepl("FLAG", COLUMN_NAME))


write_csv(pts_columns, "tables/pts_columns.csv", na="")

query = "SELECT *
    FROM ALL_TAB_COLUMNS
    WHERE OWNER = 'FDW3NF' AND IDENTITY_COLUMN='YES'
"

primary_keys = dbGetQuery(con, query)
