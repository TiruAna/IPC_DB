
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
