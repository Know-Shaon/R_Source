#https://db.rstudio.com/databases/sqlite/

install.packages("RSQLite")

# install.packages("devtools")
devtools::install_github("rstats-db/RSQLite")

library(DBI)
# Create an ephemeral in-memory RSQLite database
#con <- dbConnect(RSQLite::SQLite(), ":memory:")

#Connect to a DB on HDD
con <- dbConnect(RSQLite::SQLite(), "D:/Kaggle/HCDR/all/mydb.db")
con <- dbConnect(RSQLite::SQLite(), "D:/Kaggle/Santander/sndr.db")


#Add R Env Variables to database 
dbWriteTable(con, "mtcars", mtcars)

dbListTables(con)

dbListFields(con, "mtcars")

dbReadTable(con, "mtcars")

res <- dbSendQuery(con, "select
                   STRFTIME('%W',date(time_stamp,'+3 day')) as week,
                   sum(sales) as sales,
                   sum(units) as quantity,
                   sum(sales-cogs) as profit
                   from 
                   (select invoice_id,invoice_line,store_id,replace(time_stamp,'/','-') as time_stamp,product,units,sales,cogs from Order_Detail) o,
                   Product_Detail p
                   where
                   o.product = p.product
                   group by store_id, cat_name, week")

dbFetch(res)
