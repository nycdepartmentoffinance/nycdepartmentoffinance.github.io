# library
library(DBI)
library(odbc)
library(dplyr)
library(stringr)
library(readr)
library(readxl)

# GET ONLY PTS COLS FROM FDW TABLES

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

combined_table <- rbind(pardat, asmt, aprval) %>%
    rename(COLUMN_NAME = FLD,
           DESCRIPTION = FLDLABL) %>%
    mutate(TABLE_NAME = paste0("VW_PTS_", TBLE)) %>%
    select(TABLE_NAME, COLUMN_NAME, DESCRIPTION)


userfield_combined <- usrfield %>%
    rename(COLUMN_NAME = FLD,
           DESCRIPTION = FLDLABL) %>%
    mutate(TABLE_NAME = paste0("VW_PTS_", TBLE)) %>%
    full_join(combined_table,  by=c("TABLE_NAME", "COLUMN_NAME")) %>%
    mutate(DESCRIPTION = ifelse(!is.na(DESCRIPTION.x), DESCRIPTION.x, DESCRIPTION.y),
           NOTES = NA) %>%
    select(TABLE_NAME, COLUMN_NAME, DESCRIPTION, NOTES)



# ground truth of new page - what we want descriptions for
pts_columns <- fdw_columns %>%
    filter(TABLE_NAME %in% fdw_pts_tables) %>%
    rename(TYPE = DATA_TYPE) %>%
    select(OWNER, TABLE_NAME, COLUMN_NAME, TYPE) %>%
    left_join(userfield_combined, by=c("TABLE_NAME", "COLUMN_NAME")) %>%
    arrange(TABLE_NAME, COLUMN_NAME)


# use manual to update main table
pts_columns_manual <- readxl::read_excel("tables/pts_columns_manual_add.xlsx")

updated = pts_columns %>%
    full_join(pts_columns_manual) %>%
    group_by(OWNER, TABLE_NAME, COLUMN_NAME) %>%
    arrange(DESCRIPTION) %>%
    slice(1)


# add condbase and newbase in


text_table_converted <- readxl::read_excel("tables/text_table_converted.xlsx")



condbase_newbase <- fdw_columns %>%
    filter(TABLE_NAME %in% c("VW_PDM_CONDBASE", "VW_PDM_NEWBASE")) %>%
    left_join(text_table_converted, by=c("TABLE_NAME", "COLUMN_NAME")) %>%
    select(OWNER, TABLE_NAME, COLUMN_NAME, DATA_TYPE, DESCRIPTION) %>%
    rename(TYPE=DATA_TYPE)


updated_with_condbase_newbase <- rbind(updated, condbase_newbase) %>%
    ungroup() %>%
    select(-NOTES, -OWNER) %>%
    arrange(TABLE_NAME, COLUMN_NAME)


write.csv(updated_with_condbase_newbase, "tables/pts_columns.csv", na="")






