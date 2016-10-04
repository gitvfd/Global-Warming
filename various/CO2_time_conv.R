dataCO2 <- read.csv("/Users/vfduclos/Dropbox/viz/DPLIVECO2.csv",sep=",")
summary(dataCO2)
dataCO2$TIME <- sub("^","Jan-", dataCO2$TIME)


write.table(dataCO2, file = "/Users/vfduclos/Dropbox/viz/DPLIVECO2_new.csv", sep = ",", row.names = FALSE,quote=TRUE)
