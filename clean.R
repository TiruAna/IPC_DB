
#################### This script is used for cleaning the files obtained from webscraping #####################

library(dplyr)

# read csv files
files <- list.files()
pos <- grep(pattern = "carrefour", x = files)
files <- files[pos]
csv <- grep(pattern = ".csv", x = files)
files <- files[csv]

dbf_list <- lapply(files, function(x) suppressMessages(read.csv(file=x, stringsAsFactors = FALSE)))

for (i in seq(dbf_list)) assign(files[i], dbf_list[[i]])


clean_colnames <- function (df) {
  coln <- names(df)
  poscol <- grep("(reducere|robotname|robotversion|sortimente)", tolower(coln))
  df <- df[-poscol]
  names(df)[1] <- "Denumire"
  names(df)[2] <- "Pret"
  names(df)[3] <- "Data"
  df$Descriere <- NA
  df$idMag <- 1
  df$Um <- NA
  df <- df[,c(1,4,2,6,3,5)] #pot sa adaug numele coloanelor
  df <- df[!duplicated(df$Denumire), ]
  
  return (df)
}

clean_data <- function (df) {
  df <- clean_colnames(df)
  y <- substr(df$Data, 1, 4)
  m <- substr(df$Data, 5, 6)
  d <- substr(df$Data, 7, 8)
  
  df$Data <- paste(y, m, d, sep = "-")
  getum <- strsplit(df$Pret, "/")
  um <- ""
  for (i in 1:length(getum)) {
    um[i] <- getum[[i]][2]
  }
  df$Um <- um
  
  df$Pret <- gsub("([0-9]+).*$", "\\1", df$Pret)
  n <- substr(df$Pret, 1, nchar(df$Pret)-2)
  z <- substr(df$Pret, nchar(df$Pret)-1, nchar(df$Pret))
  df$Pret <- paste(n, z, sep = ".")
  df$Pret <- as.numeric(df$Pret)
  df$Denumire <- gsub(",","", df$Denumire)
  
  return (df)
}


match_sort <- function (df) {
  colmag <- grep(pattern = "idMag", x = names(df))
  if (length(colmag) == 0) {
    df <- clean_data(df)
  }
  colsort <- grep(pattern = "idSort", x = names(df))
  if (length(colsort) == 0) {
    df <- left_join(df, carrefour_codificat.csv[,c(1,5)], by = c("Denumire"="nume"))
  }
  
  return(df)
}


js <- grep(pattern = "jsprodusv1", x = files)
files <- files[js]
for (i in files) assign(i, match_sort(get(i)))

path <- paste0(getwd(), "/OK")
for (i in files) write.table(get(i), file = paste0(path, "/", i), sep = ",", col.names = FALSE, row.names = FALSE)  





# Populate tables

conn <- dbConnect(RSQLite::SQLite(), "C:/sqlite/IPC.db")

dbSendQuery(conn, 'INSERT INTO magazine (idMag, Nume, Adresa) VALUES (:idMag, :Nume, :Adresa);', magazine)

dbSendQuery(conn, 'INSERT INTO produse (Denumire, Descriere, Pret, Um, Data, idMag, idSort) VALUES (:Denumire, :Descriere, :Pret, :Um, :Data, :idMag, :idSort);', df)

dbListTables(conn)
dbGetQuery(conn, "DELETE FROM magazine")
dbGetQuery(conn, "SELECT * FROM magazine")
dbGetQuery(conn, "SELECT * FROM produse LIMIT 10")
t <- dbReadTable(conn, "produse")


