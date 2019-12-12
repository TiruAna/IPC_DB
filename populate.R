library(RSQLite)
#################### This script is used to populate the tables from IPC database #####################

ipcdb <- dbConnect(RSQLite::SQLite(), "IPC.db")

# 1. Populate standard tables: magazine, sortimente, posturi
magazine <- read.csv("magazine.csv", stringsAsFactors = FALSE)
posturi <- read.csv("posturi.csv", stringsAsFactors = FALSE)
sortimente <- read.csv("sortimente.csv", stringsAsFactors = FALSE)

dbSendQuery(ipcdb, 'INSERT INTO magazine (idMag, Nume, Adresa) VALUES (:idMag, :Nume, :Adresa);', magazine)
dbSendQuery(ipcdb, 'INSERT INTO posturi (idPost, Nume) VALUES (:idPost, :Nume);', posturi)
dbSendQuery(ipcdb, 'INSERT INTO sortimente (idSort, Denumire, Grupa, idPost) VALUES (:idSort, :Denumire, :Grupa, :idPost);', sortimente)

dbGetQuery(ipcdb, "SELECT * FROM magazine")
dbGetQuery(ipcdb, "SELECT * FROM posturi")
dbGetQuery(ipcdb, "SELECT * FROM sortimente")
dbGetQuery(ipcdb, "SELECT * FROM produse")

# 2. Populate the table "produse"
# First insert
df <- cbind(idProduse = c(1), `20170803carrefour.jsprodusv1.csv`[1,])
dbSendQuery(ipcdb, 'INSERT INTO produse (idProduse, Denumire, Descriere, Pret, Um, Data, idMag, idSort) VALUES (:idProduse, :Denumire, :Descriere, :Pret, :Um, :Data, :idMag, :idSort);', df)

files <- list.files()
csv <- grep(pattern = ".csv", x = files)
files <- files[csv]
prod <- grep(pattern = "produsv1", x = files)
cod <- grep(pattern = "codificat", x = files)
files <- files[c(prod, cod)]

fl <- files[prod] 
for (i in fl) {
  dbSendQuery(ipcdb, 'INSERT INTO produse (Denumire, Descriere, Pret, Um, Data, idMag, idSort) VALUES (:Denumire, :Descriere, :Pret, :Um, :Data, :idMag, :idSort);', i)
}

dbSendQuery(ipcdb, 'INSERT INTO produse (Denumire, Descriere, Pret, Um, Data, idMag, idSort) VALUES (:Denumire, :Descriere, :Pret, :Um, :Data, :idMag, :idSort);', `20170906m_image.jsprodusv1.csv`)
dbGetQuery(ipcdb, "delete from produse")
