
#################### This script is used to populate the tables from IPC database #####################

ipcdb <- dbConnect(RSQLite::SQLite(), "IPC.db")

# 1. Populate standard tables: magazine, sortimente, posturi
magazine <- read.csv("magazine.csv", stringsAsFactors = FALSE)
posturi <- read.csv("posturi.csv", stringsAsFactors = FALSE)
sortimente <- read.csv("sortimente.csv", stringsAsFactors = FALSE)

dbSendQuery(ipcdb, 'INSERT INTO magazine (idMag, Nume, Adresa) VALUES (:idMag, :Nume, :Adresa);', magazine)
dbSendQuery(ipcdb, 'INSERT INTO posturi (idPost, Nume) VALUES (:idPost, :Nume);', posturi)
dbSendQuery(ipcdb, 'INSERT INTO sortimente (idSort, Denumire, Grupa, idPost) VALUES (:idSort, :Denumire, :Grupa, :idPost);', sortimente)

dbGetQuery(conn, "SELECT * FROM magazine")
dbGetQuery(conn, "SELECT * FROM posturi")
dbGetQuery(conn, "SELECT * FROM sortimente")


# 2. Populate the table "produse"

