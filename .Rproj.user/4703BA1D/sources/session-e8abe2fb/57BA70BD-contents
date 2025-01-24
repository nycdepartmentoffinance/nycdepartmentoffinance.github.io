table = "CAMA_SALEHIST_HIST"


get_record_count = function(con, table){
    query <-
        glue::glue("
    SELECT count(*)
      FROM FDW3NF.{table};
    ")

    count = dbGetQuery(con, query)
    num = as.numeric(count[1])

    return(count)
}


get_record_count(con, "VW_PTS_APRVAL")
