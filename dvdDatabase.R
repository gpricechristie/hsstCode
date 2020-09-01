install.packages("RPostgreSQL")

require("RPostgreSQL")

drv = dbDriver("PostgreSQL")

con = dbConnect(drv, dbname = "xfllavhl", host = "kandula.db.elephantsql.com", port = 5432,
                user = "xfllavhl", password = "4OMEvZErvBwyvp-Ex-qbrKXfn0hiCbkP")

query = dbGetQuery(con, "SELECT * 
                        FROM customer cust")

query = dbGetQuery(con, "SELECT * 
                        FROM customer cust
                        WHERE cust.first_name ILIKE \'Dave\'")

query = dbGetQuery(con, "SELECT * 
                        FROM customer cust
                        WHERE cust.first_name ILIKE \'Dav%\'")

query = dbGetQuery(con, "SELECT * 
                        FROM customer cust
                        ORDER BY cust.first_name")

query = dbGetQuery(con, "SELECT * 
                        FROM customer cust
                        ORDER BY cust.first_name")

query = dbGetQuery(con, "SELECT * 
                        FROM customer cust
                        JOIN rental rent on cust.customer_id = rent.customer_id
                        WHERE cust.first_name ILIKE \'Dave\'")

query = dbGetQuery(con, "SELECT cust.first_name, cust.last_name,
                        film.title
                        FROM customer cust
                        JOIN rental rent on cust.customer_id = rent.customer_id
                        JOIN inventory inv on rent.inventory_id = inv.inventory_id
                        JOIN film film on film.film_id = inv.film_id
                        WHERE film.title ILIKE \'%Whale%\'
                        ORDER BY cust.first_name, cust.last_name,
                        film.title")






