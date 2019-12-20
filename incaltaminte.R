# incaltaminte

##################### Batta ###################################################
###############################################################################
rm(list=ls())
library(dplyr)
library(stringr)
#read csv files
files <- list.files()
csv <- grep(pattern = ".csv", x = files)
files <- files[csv]
prod <- grep(pattern = "produsv1", x = files)
cod <- grep(pattern = "codificat", x = files)
files <- files[c(prod, cod)]

dbf_list <- lapply(files, function(x) suppressMessages(read.csv(file=x, stringsAsFactors = FALSE)))
for (i in seq(dbf_list)) assign(files[i], dbf_list[[i]])

add_idmag <- function(df) {
  x <- 0 
  if (df$RobotName[1] == "batta") {
    x <- 7
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
  poscol <- grep("(reducere|robotname|robotversion|pret_vechi|cod)", coln)
  if (length(poscol) != 0) {
    df <- df[-poscol]
    coln <- tolower(names(df))
  }
  colnames(df)[which(coln == "nume")] <- "Denumire"
  colnames(df)[which(coln == "pret_nou")] <- "Pret"
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
    if (df$idMag[1] == 7) {
      df <- left_join(df, bata_codificat.csv[,c(1,12)], by = c("Denumire"="Nume"))
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


fl <- files[prod]
for (i in fl) {
  print(i)
  assign(i, match_sort(get(i)))
}

##################### Benvenuti ###################################################
###################################################################################
rm(list=ls())
library(dplyr)
library(stringr)
#read csv files
files <- list.files()
csv <- grep(pattern = ".csv", x = files)
files <- files[csv]
prod <- grep(pattern = "sortimentv1", x = files)
cod <- grep(pattern = "codificat", x = files)
files <- files[c(prod, cod)]

dbf_list <- lapply(files, function(x) suppressMessages(read.csv(file=x, stringsAsFactors = FALSE)))
for (i in seq(dbf_list)) assign(files[i], dbf_list[[i]])

add_cod <- function (df) {
  df$Nume <- paste(df$Nume, " Cod: ",df$Cod)
  return (df)
}

fl <- files[prod]
for (i in fl) {
  print(i)
  assign(i, add_cod(get(i)))
}

add_idmag <- function(df) {
  x <- 0 
  if (df$RobotName[1] == "benvenuti") {
    x <- 4
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
  poscol <- grep("(reducere|robotname|robotversion|cod)", coln)
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
  if (length(split_price[[1]])>1) {
    um <- ""
    for (i in 1:length(split_price)) {
      um[i] <- split_price[[i]][2]
    }
  } else {
    um <- NA
  }
  df$Pret <- price
  df$Um <- um
  
  # if (grepl("\\.", x = df$Pret[1])) {    # if
  #   df$Pret <- as.numeric(df$Pret)
  # } else {
  #   n <- substr(df$Pret, 1, nchar(df$Pret)-2)
  #   z <- substr(df$Pret, nchar(df$Pret)-1, nchar(df$Pret))
  #   df$Pret <- paste(n, z, sep = ".")
  #   df$Pret <- as.numeric(df$Pret)
  # }
  # 
  # # df$Pret <- gsub("([0-9]+).*$", "\\1", df$Pret)
  # 
  # df$Denumire <- gsub(",","", df$Denumire)
  
  return (df)
}


match_sort <- function (df) {
  colmag <- grep(pattern = "idMag", x = names(df))
  if (length(colmag) == 0) {
    df <- get_price(df)
  }
  colsort <- grep(pattern = "idSort", x = names(df))
  if (length(colsort) == 0) {
    if (df$idMag[1] == 4) {
      df <- left_join(df, benvenuti_codificat.csv[,c(1,2)], by = c("Denumire"="Nume"))
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


fl <- files[prod]
for (i in fl) {
  print(i)
  assign(i, match_sort(get(i)))
}


##################### Deichman ###################################################
##################################################################################


rm(list=ls())
library(dplyr)
library(stringr)
#read csv files
files <- list.files()
csv <- grep(pattern = ".csv", x = files)
files <- files[csv]
prod <- grep(pattern = "sortimentv1", x = files)
cod <- grep(pattern = "codificat", x = files)
files <- files[c(prod, cod)]

dbf_list <- lapply(files, function(x) suppressMessages(read.csv(file=x, stringsAsFactors = FALSE)))
for (i in seq(dbf_list)) assign(files[i], dbf_list[[i]])

add_cod <- function (df) {
  df$Nume <- paste(df$Nume, " Cod: ",df$Cod)
  return (df)
}

fl <- files[prod]
for (i in fl) {
  print(i)
  assign(i, add_cod(get(i)))
}

add_idmag <- function(df) {
  x <- 5 
  # if (df$RobotName[1] == "benvenuti") {
  #   x <- 5
  # } else if (df$RobotName[1] == "cora") {
  #   x <- 2
  # } else if (df$RobotName[1] == "m_image.js") {
  #   x <- 3
  # }
  return (x)
}

clean_colnames <- function (df) {
  coln <- tolower(names(df))
  idmag <- add_idmag(df)
  poscol <- grep("(reducere|robotname|robotversion|cod)", coln)
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
  if (length(split_price[[1]])>1) {
    um <- ""
    for (i in 1:length(split_price)) {
      um[i] <- split_price[[i]][2]
    }
  } else {
    um <- NA
  }
  df$Pret <- price
  df$Um <- um
  
  # if (grepl("\\.", x = df$Pret[1])) {    # if
  #   df$Pret <- as.numeric(df$Pret)
  # } else {
  #   n <- substr(df$Pret, 1, nchar(df$Pret)-2)
  #   z <- substr(df$Pret, nchar(df$Pret)-1, nchar(df$Pret))
  #   df$Pret <- paste(n, z, sep = ".")
  #   df$Pret <- as.numeric(df$Pret)
  # }
  # 
  # # df$Pret <- gsub("([0-9]+).*$", "\\1", df$Pret)
  # 
  # df$Denumire <- gsub(",","", df$Denumire)
  
  return (df)
}


match_sort <- function (df) {
  colmag <- grep(pattern = "idMag", x = names(df))
  if (length(colmag) == 0) {
    df <- get_price(df)
  }
  colsort <- grep(pattern = "idSort", x = names(df))
  if (length(colsort) == 0) {
    if (df$idMag[1] == 5) {
      df <- left_join(df, deichman_codificat.csv[,c(1,2)], by = c("Denumire"="Nume"))
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


fl <- files[prod]
for (i in fl) {
  print(i)
  assign(i, match_sort(get(i)))
}


##################### Leonardo ###################################################
##################################################################################


rm(list=ls())
library(dplyr)
library(stringr)
#read csv files
files <- list.files()
csv <- grep(pattern = ".csv", x = files)
files <- files[csv]
prod <- grep(pattern = "sortimentv1", x = files)
cod <- grep(pattern = "codificat", x = files)
files <- files[c(prod, cod)]

dbf_list <- lapply(files, function(x) suppressMessages(read.csv(file=x, stringsAsFactors = FALSE)))
for (i in seq(dbf_list)) assign(files[i], dbf_list[[i]])


add_idmag <- function(df) {
  x <- 8 
  # if (df$RobotName[1] == "benvenuti") {
  #   x <- 5
  # } else if (df$RobotName[1] == "cora") {
  #   x <- 2
  # } else if (df$RobotName[1] == "m_image.js") {
  #   x <- 3
  # }
  return (x)
}

clean_colnames <- function (df) {
  coln <- tolower(names(df))
  idmag <- add_idmag(df)
  poscol <- grep("(reducere|robotname|robotversion|cod|pret_vechi)", coln)
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
  if (length(split_price[[1]])>1) {
    um <- ""
    for (i in 1:length(split_price)) {
      um[i] <- split_price[[i]][2]
    }
  } else {
    um <- NA
  }
  df$Pret <- price
  df$Um <- um
  
  # if (grepl("\\.", x = df$Pret[1])) {    # if
  #   df$Pret <- as.numeric(df$Pret)
  # } else {
  #   n <- substr(df$Pret, 1, nchar(df$Pret)-2)
  #   z <- substr(df$Pret, nchar(df$Pret)-1, nchar(df$Pret))
  #   df$Pret <- paste(n, z, sep = ".")
  #   df$Pret <- as.numeric(df$Pret)
  # }
  # 
  # # df$Pret <- gsub("([0-9]+).*$", "\\1", df$Pret)
  # 
  # df$Denumire <- gsub(",","", df$Denumire)
  
  return (df)
}


match_sort <- function (df) {
  colmag <- grep(pattern = "idMag", x = names(df))
  if (length(colmag) == 0) {
    df <- get_price(df)
  }
  colsort <- grep(pattern = "idSort", x = names(df))
  if (length(colsort) == 0) {
    if (df$idMag[1] == 8) {
      df <- left_join(df, leonardo_codificat.csv[,c(1,2)], by = c("Denumire"="Nume"))
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


fl <- files[prod]
for (i in fl) {
  print(i)
  assign(i, match_sort(get(i)))
}
