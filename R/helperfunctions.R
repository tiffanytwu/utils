#### Various user defined functions

#Google BigQuery
library(bigrquery)

#Helper function to delete time partitioned tables
BQ_delete <- function(project, dataset, table, start, end){
  Sys.sleep(.5)
  tables <- paste(table, format.Date(seq(as.Date(start), as.Date(end), 1), '%Y%m%d'), sep="_")
  for(i in 1:length(tables)){
    tryCatch(delete_table(project, dataset, tables[i]),
             error=function(e) {print(message("ERROR: ", e))},
             warning=function(cond) {message("WARNING: ", cond)}
             finally = message("Table deleted:", tables[i]))
  }
}

#Helper function to copy time partitioned tables
BQ_copy <- function(src_project, src_dataset, src_table, start, end, 
                    dest_project=src_project, dest_dataset=src_dataset, dest_table=src_table){
  src_tables <- paste(src_table, format.Date(seq(as.Date(start), as.Date(end), 1), '%Y%m%d'), sep="_")
  dest_tables <- paste(dest_table, format.Date(seq(as.Date(start), as.Date(end), 1), '%Y%m%d'), sep="_")

  for(i in 1:length(src_tables)){
    src <- list(project_id = src_project, dataset_id =  src_dataset, table_id = src_tables[i])
    dest <- list(project_id = dest_project, dataset_id =  dest_dataset, table_id = dest_tables[i])
    tryCatch(copy_table(src, dest),
             error = function(e) e,
             finally = message(src, ' to ', dest))
  }
}

#Helper function to pull data from BQ
pull_query <- function(query, proj = 'global', write_disposition='write_truncate'){

    results <- query_exec(query = query
                          ,project = proj 
                          ,max_pages = as.numeric(Inf)
                          ,write_disposition = write_disposition
                          ,destination_table = "global:test.temp"
                          ,use_legacy_sql=FALSE)
    return(results)
}
