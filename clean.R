
#################### This script is used for cleaning the files obtained from webscraping #####################
rm(list=ls())
library(dplyr)
library(stringr)

# read csv files
# files <- list.files()
# csv <- grep(pattern = ".csv", x = files)
# files <- files[csv]
# prod <- grep(pattern = "produsv1", x = files)
# cod <- grep(pattern = "codificat", x = files)
# files <- files[c(prod, cod)]

# read xlsx files
files <- list.files()
xlsx <- grep(pattern = ".xlsx", x = files)
xls <- files[xlsx]
csv <- grep(pattern = "mega_codificat", x = files)
csv <- files[csv]

dbf_list <- lapply(xls, function(x) suppressMessages(read_excel(x)))
for (i in seq(dbf_list)) assign(xls[i], dbf_list[[i]])
csv_list <- lapply(csv, function(x) suppressMessages(read.csv(file=x, stringsAsFactors = FALSE)))
for (i in seq(csv_list)) assign(csv[i], csv_list[[i]])

cleanxls <- function (df) {
  df <- as.data.frame(df)
  df <- df[,-c(1)]
  df[, ] <- lapply(df[, ], as.character)
  df$RobotName <- "m_image.js"
  return (df)
}
for (i in xls) {
  assign(i, cleanxls(get(i)))
}
# dbf_list <- lapply(files, function(x) suppressMessages(read.csv(file=x, stringsAsFactors = FALSE)))
# 
# for (i in seq(dbf_list)) assign(files[i], dbf_list[[i]])


add_idmag <- function(df) {
  x <- 0 
  if (df$RobotName[1] == "carrefour.js") {
    x <- 1
  } else if (df$RobotName[1] == "cora") {
    x <- 2
  } else if (df$RobotName[1] == "m_image.js") {
    x <- 3
  }
  return (x)
}

clean_colnames <- function (df) {
  coln <- tolower(names(df))
  idmag <- add_idmag(df)
  poscol <- grep("(reducere|robotname|robotversion)", coln)
  if (length(poscol) != 0) {
    df <- df[-poscol]
    coln <- tolower(names(df))
  }
  colnames(df)[which(coln == "nume")] <- "Denumire"
  colnames(df)[which(coln == "pret")] <- "Pret"
  colnames(df)[which(coln == "date")] <- "Data"
  
  df <- df[!duplicated(df$Denumire), ]
  
  df$Descriere <- NA
  df$Um <- NA
  df$idMag <- idmag
  
  poscoltbl <- grep("(Denumire|Descriere|Pret|Um|Data|idMag)", names(df))
  if (length(poscoltbl)!=ncol(df)) {
    col <- 1:ncol(df)
    diff <- setdiff(col,poscoltbl)
    if (length(diff) == 1) {
      df$Descriere <- paste(names(df)[diff[1]], ": ", df[,diff[1]])
    } else {
      x <- ""
      for (i in diff) {
        x <- paste(x, names(df)[i],": ", df[,i])
      }
      df$Descriere <- x
    }
  }

  df <- df[,c("Denumire", "Descriere", "Pret", "Um", "Data", "idMag")]

  return (df)
}


get_date <- function (df) {
  df <- clean_colnames(df)
  y <- substr(df$Data, 1, 4)
  m <- substr(df$Data, 5, 6)
  d <- substr(df$Data, 7, 8)
  df$Data <- paste(y, m, d, sep = "-")
  
  return(df)
}


get_price <- function (df) {
  df <- get_date(df)
  df$Pret <- as.character(df$Pret)
  split_price <- str_split(df$Pret, " ", 2)
  price <- ""
  for (i in 1:length(split_price)) {
    price[i] <- split_price[[i]][1]
  }
  if (length(split_price[[1]])) {
    um <- ""
    for (i in 1:length(split_price)) {
      um[i] <- split_price[[i]][2]
    }
  } else {
    um <- NA
  }
  df$Pret <- price
  df$Um <- um
 
  if (grepl("\\.", x = df$Pret[1])) {    # if
    df$Pret <- as.numeric(df$Pret)
  } else {
    n <- substr(df$Pret, 1, nchar(df$Pret)-2)
    z <- substr(df$Pret, nchar(df$Pret)-1, nchar(df$Pret))
    df$Pret <- paste(n, z, sep = ".")
    df$Pret <- as.numeric(df$Pret)
  }
  
  # df$Pret <- gsub("([0-9]+).*$", "\\1", df$Pret)

  df$Denumire <- gsub(",","", df$Denumire)
  
  return (df)
}


match_sort <- function (df) {
  colmag <- grep(pattern = "idMag", x = names(df))
  if (length(colmag) == 0) {
    df <- get_price(df)
  }
  colsort <- grep(pattern = "idSort", x = names(df))
  if (length(colsort) == 0) {
    if (df$idMag[1] == 1) {
      df <- left_join(df, carrefour_codificat.csv[,c(1,5)], by = c("Denumire"="nume"))
    } else if (df$idMag[1] == 2) {
      df <- left_join(df, cora_codificat.csv[,c(1,5)], by = c("Denumire"="nume"))
    } else if (df$idMag[1] == 3) {
      df <- left_join(df, mega_codificat.csv[,c(1,3)], by = c("Denumire"="nume"))
    }
  }
  pretna <- which(is.na(df$Pret))
  if (length(pretna) != 0 ) {
    df <- df[-pretna,]
  }
  df <- df[!duplicated(df$Denumire),]
  return(df)
}

# fl <- files[prod] 
# for (i in fl) {
#   print(i)
#   assign(i, match_sort(get(i)))
# }


for (i in xls) {
  print(i)
  assign(i, match_sort(get(i)))
}



# js <- grep(pattern = "jsprodusv1", x = files)
# files <- files[js]
# for (i in files) assign(i, match_sort(get(i)))
# 
# path <- paste0(getwd(), "/OK")
# for (i in files) write.table(get(i), file = paste0(path, "/", i), sep = ",", col.names = FALSE, row.names = FALSE)  
# 
# # Populate tables
# 
# conn <- dbConnect(RSQLite::SQLite(), "C:/sqlite/IPC.db")
# 
# dbSendQuery(conn, 'INSERT INTO magazine (idMag, Nume, Adresa) VALUES (:idMag, :Nume, :Adresa);', magazine)
# 
# dbSendQuery(conn, 'INSERT INTO produse (Denumire, Descriere, Pret, Um, Data, idMag, idSort) VALUES (:Denumire, :Descriere, :Pret, :Um, :Data, :idMag, :idSort);', df)
# 
# dbListTables(conn)
# dbGetQuery(conn, "DELETE FROM magazine")
# dbGetQuery(conn, "SELECT * FROM magazine")
# dbGetQuery(conn, "SELECT * FROM produse LIMIT 10")
# t <- dbReadTable(conn, "produse")

d <- duplicated(`20180716carrefour.jsprodusv1.csv`$nume)
