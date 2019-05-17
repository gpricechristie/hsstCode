#**
#*  @file     HSST_B2_DataScience_Practical2
#*  @author   Gareth Price
#*  @date     10-05-2019
#*  @version  1.0
#*
#*  Demonstration of reading data directly from database
#*

require("RPostgreSQL")

#Connect to database
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "dvdrental",
                 host = "localhost", port = 5432,
                 user = "<user>", password = '<Password>')

cwpLungTableExists = dbExistsTable(con, "RDF_ETL_CWPHeadNeck")
if(cwpLungTableExists == FALSE){
  return(NULL)
}
dvd.data = dbGetQuery(con, "SELECT cust.first_name, cust.last_name,film.title
                           FROM customer cust
                           JOIN rental rent on cust.customer_id = rent.customer_id
                           JOIN inventory inv on rent.inventory_id = inv.inventory_id
                           JOIN film film on film.film_id = inv.film_id
                           WHERE film.title ILIKE '%Whale%'
                           ORDER BY cust.first_name, cust.last_name,
                           film.title")

dbDisconnect(con)

print(dvd.data)
