#Introduction to Data Mining with R and Data Import/Export in R1
####Index

#Save and Load R Objects
#import and export CVS files
#Import from and Export to EXCEL Files
#Read from Databases


#Save and Load R Objects
a <- 1:10 
# Create a dumData.Rdata file before saving the below file
save(a, file = "E:/A N A L Y T I C S/Algorithmic Thinking/The Analytics edge/RDATA/dumData.Rdata")
rm(a)
a 
## Error in eval(expr, envir, enclos): object 'a' not found 
load("E:/A N A L Y T I C S/Algorithmic Thinking/The Analytics edge/RDATA/dumData.Rdata")
a 
## [1] 1 2 3 4 5 6 7 8 9 10


#import and export CVS files
var1 <- 1:5
var2 <- (1:5)/10
var3 <- c("R", "and", "Data Mining", "Examples", "Case Studies")
df1 <- data.frame(var1, var2, var3)
names(df1) <- c("VarInt", "VarReal", "VarChar")
# save to a csv file
write.csv(df1, "./data/dummmyData.csv", row.names = FALSE)
# read from a csv file
df2 <- read.csv("./data/dummmyData.csv")
print(df2)
## VarInt VarReal VarChar
## 1 1 0.1 R
## 2 2 0.2 and
## 3 3 0.3 Data Mining
## 4 4 0.4 Examples
## 5 5 0.5 Case Studies


#Import from and Export to EXCEL Files
library(xlsx)
xlsx.file <- "./data/dummmyData.xlsx"
write.xlsx(df2, xlsx.file, sheetName = "sheet1", row.names = F)
df3 <- read.xlsx(xlsx.file, sheetName = "sheet1")
df3
## VarInt VarReal VarChar
## 1 1 0.1 R
## 2 2 0.2 and
## 3 3 0.3 Data Mining
## 4 4 0.4 Examples
## 5 5 0.5 Case Studies


#Read from Databases
library(RODBC)
db <- odbcConnect(dsn = "servername", uid = "userid",pwd = "******")
sql <- "SELECT * FROM lib.table WHERE ..."
# or read query from file
sql <- readChar("myQuery.sql", nchars=99999)
myData <- sqlQuery(db, sql, errors=TRUE)
odbcClose(db)

