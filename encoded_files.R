
#################### This script is used to clean the manually encoded files #####################

# 1. Carrefour
m <- Model_2_codificare_carrefour_august1
m <- m[,-c(1)]
m <- m[,-c(3,6,7)]
names(m)[5] <- "idSort"
m <- m[!duplicated(m), ] #eliminare duplicate
write.csv(m,"carrefour_codificat.csv", row.names = FALSE)

# 2. Cora
m <- Model_2_codificare_cora_august
m <- m[,-c(5,6)]
names(m)[5] <- "idSort"
m <- m[!duplicated(m$nume), ] #eliminare duplicate
write.csv(m,"cora_codificat.csv", row.names = FALSE)

# 3. Mega
m <- Model_2_codificare_mega_image_august
m <- m[,-c(1)]
names(m)[3] <- "idSort"
m <- m[!duplicated(m$nume), ] #eliminare duplicate
m$idSort <- as.numeric(m$idSort)
m$Post_cheltuieli <- as.numeric(m$Post_cheltuieli)
write.csv(m,"mega_codificat.csv", row.names = FALSE)

x <- m %>% group_by(idSort) %>% summarise(n=n())

benvenuti_codificat <- benvenuti_codificat[!duplicated(benvenuti_codificat$Nume),]
benvenuti_codificat.csv$Nume <- gsub("\\s*\\w*$", "", benvenuti_codificat.csv$Nume)
benvenuti_codificat.csv$Nume <- paste(benvenuti_codificat.csv$Nume, " Cod: ",benvenuti_codificat.csv$Cod)

write.csv(benvenuti_codificat.csv, "benvenuti_codificat.csv", row.names = FALSE)


deichman_codificat.csv$Nume <- paste(deichman_codificat.csv$Nume, " Cod: ",deichman_codificat.csv$Cod)
deichman_codificat.csv$Nume <- paste(deichman_codificat.csv$Nume, " Cod: ",deichman_codificat.csv$Cod)
deichman_codificat.csv <- deichman_codificat.csv[!duplicated(deichman_codificat.csv$Nume),]
write.csv(deichman_codificat.csv, "deichman_codificat.csv", row.names = FALSE)


leonardo_codificat.csv$Nume <- paste(leonardo_codificat.csv$Nume, " Cod: ",leonardo_codificat.csv$Cod)
leonardo_codificat.csv$Nume <- paste(leonardo_codificat.csv$Nume, " Cod: ",leonardo_codificat.csv$Cod)
leonardo_codificat.csv <- leonardo_codificat.csv[!duplicated(leonardo_codificat.csv$Nume),]
write.csv(leonardo_codificat.csv, "leonardo_codificat.csv", row.names = FALSE)

zara_codificat.csv$Nume <- paste(zara_codificat.csv$Nume, " Cod: ", zara_codificat.csv$Cod)
zara_codificat.csv <- zara_codificat.csv[!duplicated(zara_codificat.csv$Nume),]
zara_codificat.csv$Nume <- as.character(zara_codificat.csv$Nume)
write.csv(zara_codificat.csv, "zara_codificat.csv", row.names = FALSE)
