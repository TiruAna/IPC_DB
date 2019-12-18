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
df <- data.frame(idProduse = c(1), Denumire = c("test"), Descriere = c("test"), Pret = c(0), Um = c("test"), Data = c("2019-12-12"), idMag = c(1), idSort = c(NA))
dbSendQuery(ipcdb, 'INSERT INTO produse (idProduse, Denumire, Descriere, Pret, Um, Data, idMag, idSort) VALUES (:idProduse, :Denumire, :Descriere, :Pret, :Um, :Data, :idMag, :idSort);', df)


files <- list.files()
csv <- grep(pattern = ".csv", x = files)
files <- files[csv]
prod <- grep(pattern = "produsv1", x = files)
cod <- grep(pattern = "codificat", x = files)
files <- files[c(prod, cod)]




fl <- files[prod] 
for (i in xls) {
  print(i)
  ipcdb <- dbConnect(RSQLite::SQLite(), "IPC.db")
  try(dbSendQuery(ipcdb, 'INSERT INTO produse (Denumire, Descriere, Pret, Um, Data, idMag, idSort) VALUES (:Denumire, :Descriere, :Pret, :Um, :Data, :idMag, :idSort);', get(i)))
}




dbGetQuery(ipcdb, "SELECT count(*) FROM produse")
dbGetQuery(ipcdb, "SELECT * FROM produse")
dbSendQuery(ipcdb, 'INSERT INTO produse (Denumire, Descriere, Pret, Um, Data, idMag, idSort) VALUES (:Denumire, :Descriere, :Pret, :Um, :Data, :idMag, :idSort);', `20170906m_image.jsprodusv1.csv`)
dbGetQuery(ipcdb, "delete from produse")

dbGetQuery(ipcdb, "SELECT data, count(idProduse) FROM produse group by data")
dbGetQuery(ipcdb, "SELECT idMag, count(idProduse) FROM produse group by idMag")
dbGetQuery(ipcdb, "SELECT strftime('%d',data) from produse limit 10")

x = dbGetQuery(ipcdb, "SELECT * FROM produse where data == \"2017-09-16\"")
cx <- left_join(`20180713carrefour.jsprodusv1.csv`, x, by = c("Denumire" = "Denumire"))

sel <- dbGetQuery(ipcdb, "SELECT * FROM produse where denumire == \"Rata tanara eviscerata fara organe Kacsa per kg\"")
dbGetQuery(ipcdb, "SELECT * FROM produse where idProduse == 293348")
dbGetQuery(ipcdb, "DELETE * FROM produse where DATA == \"2017-09-20\"")
