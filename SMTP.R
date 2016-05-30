
source('../QAR/00_RProj/00_Global/iSynGeneral.R')

dtSMTP <- fread("./raw_data/SMTP_EMAIL.csv")

dtSTORE <- dtSMTP[toupper(REMARK) == "STORE", 
                  c("WERKS","SMTP_ADDR"), with = FALSE] 
setnames(dtSTORE, "SMTP_ADDR", "G1SMTPADR")
dtRGMAN <- dtSMTP[toupper(REMARK) == "REGMAN", 
                  c("WERKS","SMTP_ADDR"), with = FALSE] 
setnames(dtRGMAN, "SMTP_ADDR", "G1SMTPREG")
dtSTMAN <- dtSMTP[toupper(REMARK) == "STOREMAN", 
                  c("WERKS","SMTP_ADDR"), with = FALSE] 
setnames(dtSTMAN, "SMTP_ADDR", "G1SMTPMNG")
dt__LAB <- dtSMTP[toupper(REMARK) == "LAB", 
                  c("WERKS","SMTP_ADDR"), with = FALSE] 
setnames(dt__LAB, "SMTP_ADDR", "G1SMTPLAB")

dtPLANT <- merge(dtSTORE, dtRGMAN, by = "WERKS", all = TRUE)
dtPLANT <- merge(dtPLANT, dtSTMAN, by = "WERKS", all = TRUE)
dtPLANT <- merge(dtPLANT, dt__LAB, by = "WERKS", all = TRUE)

dtPLANT[, WERKS:= str_pad(WERKS, 4, pad ="0")]
setnames(dtPLANT, "WERKS", "PLANT")
dtPLANT[, `:=`(ALTITUDE = 0, LATITUDE = 0, LONGITUDE = 0 )]

write.table(dtPLANT, "c:/FTP/SMTP_GEO.csv", sep=";", na= "", 
            row.names = FALSE, col.names = TRUE)

dtSMTP <- fread("c:/FTP/SMTP_GEO2.csv")
dtSMTP[, PLANT:= str_pad(PLANT, 4, pad ="0")]
dtGEO  <- fread("c:/FTP/GEO.csv", colClasses = "character")


dtSMTP_GEO <- merge(dtSMTP[, .(PLANT, G1SMTPADR, G1SMTPREG, G1SMTPMNG, G1SMTPLAB)],
                    dtGEO, by = "PLANT", all.x =TRUE)
setcolorder(dtSMTP_GEO, c("PLANT", 
                          "G1SMTPADR", "G1SMTPREG", "G1SMTPMNG", "G1SMTPLAB",
                          "ALTITUDE",  "LATITUDE",  "LONGITUDE" ) )
write.table(dtSMTP_GEO, "c:/FTP/SMTP_GEO.csv", sep=";", na= "", 
            row.names = FALSE, col.names = TRUE)
