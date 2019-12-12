install.packages("RSQLite")
library(RSQLite)
#################### This script is used to create the IPC database  #####################

ipcdb <- dbConnect(RSQLite::SQLite(), "IPC.db")

dbSendQuery(conn = ipcdb, 
            "CREATE TABLE magazine (
              idMag INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
              Nume VARCHAR(45) NOT NULL,
              Adresa VARCHAR(45) NOT NULL,
              CONSTRAINT idMag_UNIQUE
              UNIQUE(idMag),
              CONSTRAINT Nume_UNIQUE
              UNIQUE(Nume),
              CONSTRAINT Adresa_UNIQUE
              UNIQUE(Adresa))"
            )


dbSendQuery(conn = ipcdb, 
            "CREATE TABLE posturi (
              idPost INTEGER PRIMARY KEY NOT NULL,
              Nume VARCHAR(1000) NOT NULL)"
)

dbSendQuery(conn = ipcdb, 
            "CREATE TABLE sortimente (
              idSort INTEGER PRIMARY KEY NOT NULL,
              Denumire VARCHAR(1000) NOT NULL,
              Grupa INTEGER NOT NULL,
              idPost INTEGER NOT NULL,
              CONSTRAINT fk_Sortimente_Posturi
              FOREIGN KEY(idPost)
              REFERENCES posturi(idPost))"
            )


dbSendQuery(conn = ipcdb, 
            "CREATE TABLE produse(
              idProduse INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
              Denumire VARCHAR(45) NOT NULL,
              Descriere VARCHAR(1000),
              Pret REAL(10,2) NOT NULL,
              Um VARCHAR(45),
              Data varchar(45) NOT NULL,
              idMag INTEGER NOT NULL,
              idSort INTEGER,
              CONSTRAINT idProduse_UNIQUE
              UNIQUE(idProduse),
              CONSTRAINT fk_Produse_Magazine
              FOREIGN KEY(idMag)
              REFERENCES magazine(idMag),
              CONSTRAINT fk_Produse_Sortimente
              FOREIGN KEY(idSort)
              REFERENCES sortimente(idSort))"
           )

dbListTables(ipcdb)

dbDisconnect(ipcdb)
#unlink("my-db.sqlite")