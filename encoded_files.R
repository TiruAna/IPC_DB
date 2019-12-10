
#################### This script is used to clean the manually encoded files #####################

m <- Model_2_codificare_carrefour_august1
m <- m[,-c(1)]
m <- m[,-c(3,6,7)]
names(m)[5] <- "idSort"
m <- m[!duplicated(m), ] #eliminare duplicate
carrefour_codificat.csv <- carrefour_codificat.csv[!duplicated(carrefour_codificat.csv$nume), ]
write.csv(carrefour_codificat.csv,"carrefour_codificat.csv")
