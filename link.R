library(data.table)
require(bit64)

link <- fread("~/RW/GV_HIST/RAW_DATA/link01.txt")
setnames(link,
         old = 1:6,
         new = c("del", "MGH2", "LNK", "RSN", "TN", "del2"))
link[, `:=`(del=NULL, del2=NULL, CNT=1, MGH2 = paste0("M", MGH2))]
LINK <- link

link <- fread("~/RW/GV_HIST/RAW_DATA/link02.txt")
setnames(link,
         old = 1:6,
         new = c("del", "MGH2", "LNK", "RSN", "TN", "del2"))
link[, `:=`(del=NULL, del2=NULL, CNT=1, MGH2 = paste0("M", MGH2))]
LINK <- rbind(LINK, link)

link <- fread("~/RW/GV_HIST/RAW_DATA/link03.txt")
setnames(link,
         old = 1:6,
         new = c("del", "MGH2", "LNK", "RSN", "TN", "del2"))
link[, `:=`(del=NULL, del2=NULL, CNT=1, MGH2 = paste0("M", MGH2))]
LINK <- rbind(LINK, link)

link <- fread("~/RW/GV_HIST/RAW_DATA/link04.txt")
setnames(link,
         old = 1:6,
         new = c("del", "MGH2", "LNK", "RSN", "TN", "del2"))
link[, `:=`(del=NULL, del2=NULL, CNT=1, MGH2 = paste0("M", MGH2))]
LINK <- rbind(LINK, link)

link <- fread("~/RW/GV_HIST/RAW_DATA/link05.txt")
setnames(link,
         old = 1:6,
         new = c("del", "MGH2", "LNK", "RSN", "TN", "del2"))
link[, `:=`(del=NULL, del2=NULL, CNT=1, MGH2 = paste0("M", MGH2))]
LINK <- rbind(LINK, link)

dtLcon <- LINK[, .N, by = .(TN, MGH2)] 
setkey(dtLcon, TN)

dtMGH2 <- data.table(MGH2= unique(dtLcon$MGH2), key = "MGH2")
dtMGH2[, BIT:= 2^(0:9)]
dtLcon <- dtMGH2[dtLcon, on = "MGH2"]

dtTNBIT <- dtLcon[, .(BIT = sum(BIT)), by = .(TN)]
LINK <- dtTNBIT[LINK, on="TN"]
setkey(LINK, "TN")

dtIOBJ <- dcast(LINK, BIT ~ MGH2, sum)

############### Generate WGHCMB ###############################################
x <- c(0, 1)
dtIOBJ <- as.data.table(
  expand.grid(list(M100 = x * 2^0, 
                   M110 = x * 2^1, 
                   M120 = x * 2^2,
                   M130 = x * 2^3,
                   M140 = x * 2^4,
                   M150 = x * 2^5,
                   M160 = x * 2^6,
                   M170 = x * 2^7,
                   M180 = x * 2^8,
                   M190 = x * 2^9)))

dtIOBJ[, G1WGHCMB:=rowSums(.SD)]

dtIOBJ[M100 != 0 & M110 + M120 + M130 + M140 + M150 + M160 + M170 + M180 + M190 == 0, LNK:= "01"]
table(dtIOBJ$LNK)
dtIOBJ[M100 != 0 & M110 >= 0 & M120 >= 0 & M130 != 0 & M140 == 0, LNK:= "2A"]
table(dtIOBJ$LNK)
dtIOBJ[M100 != 0 & M110 >= 0 & M120 >= 0 & M130 == 0 & M140 != 0, LNK:= "2B"]
table(dtIOBJ$LNK)
dtIOBJ[M100 != 0 & M110+M120+M150+M160+M170+M180+M190 > 0 & M130 == 0 & M140 == 0, LNK:= "2C"]
table(dtIOBJ$LNK)
dtIOBJ[M100 != 0 & M110 >= 0 & M120 >= 0 & M130 != 0 & M140 != 0, LNK:= "2D"]
table(dtIOBJ$LNK)
dtIOBJ[M100 == 0 & M110 == 0 & M120 >= 0 & M130 >= 0 & M140 >= 0, LNK:= "03"]
table(dtIOBJ$LNK)
dtIOBJ[M100 == 0 & M110 != 0 & M120 == 0 & M130 >= 0 & M140 >= 0, LNK:= "04"]
table(dtIOBJ$LNK)
dtIOBJ[M100 == 0 & M110 != 0 & M120 != 0 & M130 >= 0 & M140 >= 0, LNK:= "05"]
table(dtIOBJ$LNK)

View(dtIOBJ[is.na(LNK)])

# for (col in 1:10) set(dtIOBJ, which(dtIOBJ[[col]] != 0), col, 1)
dtIOBJ <- dtIOBJ[, lapply(.SD, as.character)]
for (col in 1:10) set(dtIOBJ, which(dtIOBJ[[col]] != "0"), col, "Y")
for (col in 1:10) set(dtIOBJ, which(dtIOBJ[[col]] == "0"), col, "N")

setcolorder(dtIOBJ, c("G1WGHCMB", 
                      "M100", "M110", "M120", "M130",
                      "M140", "M150", "M160", "M170",
                      "M180", "M190", "LNK"))

write.table(x= dtIOBJ, file= "C:/FTP/WGH_COMB.csv", quote = TRUE, 
            sep = ";", row.names = FALSE, col.names = TRUE)

###############################
empty <- "    "
dtIOBJ <- as.data.table(
  expand.grid(list(M100 = c(empty, "FLNS"), 
                   M110 = c(empty, "CLNS"),
                   M120 = c(empty, "CSOL"),
                   M130 = c(empty, "FRMS"),
                   M140 = c(empty, "SNGL"),
                   M150 = c(empty, "OART"),
                   M160 = c(empty, "SRVS"),
                   M170 = c(empty, "EQPM"),
                   M180 = c(empty, "PRNM"),
                   M190 = c(empty, "AART"))))

lLTX <- do.call("paste", c(dtIOBJ, sep = ","))

empty <- "   "
dtIOBJ <- as.data.table(
  expand.grid(list(M100 = c(empty, "100"), 
                   M110 = c(empty, "110"),
                   M120 = c(empty, "120"),
                   M130 = c(empty, "130"),
                   M140 = c(empty, "140"),
                   M150 = c(empty, "150"),
                   M160 = c(empty, "160"),
                   M170 = c(empty, "170"),
                   M180 = c(empty, "180"),
                   M190 = c(empty, "190"))))

lMDX <- do.call("paste", c(dtIOBJ, sep = ","))

dtIOBJT <- data.table(G1WGHCMB = 0:1023, MTXT = lMDX, LTXT = lLTX)

write.table(x= dtIOBJT, file= "C:/FTP/WGH_COMBT.csv", quote = TRUE, 
            sep = ";", row.names = FALSE, col.names = TRUE)




lnkc <- dcast(LINK, TN+RSN+LNK ~ MGH2, sum)
lnk2 <- lnkc[, lapply(.SD, sum), by = "LNK", .SDcols=4:13] 
setkey(lnk2, LNK)
View(lnk2)
View(lnkc[TN %in% lnkc[LNK == "2A" & M140 > 0]$TN])
lnk3 <- lnkc[, lapply(.SD, sum), by = c("LNK","TN"), .SDcols=4:13] 

###### Generate AAGCMB
dtKTGRM      <- fread("c:/SAPexport/KTGRM.csv")

dtTVKMT      <- fGetEXPTable(pTable  = "TVKMT", pKey = c("KTGRM"),
                             pSystID = pECC_SYST, pClient = "300")
dtTVKMT      <- dtTVKMT[ SPRAS == "E", .(KTGRM, VTEXT)]

x <- c(0, 1)
dtIOBJ <- as.data.table(
  expand.grid(list(Z1 = x * 2^0, 
                   Z2 = x * 2^1, 
                   Z3 = x * 2^2,
                   Z4 = x * 2^3,
                   Z5 = x * 2^4,
                   Z6 = x * 2^5,
                   Z7 = x * 2^6,
                   Z8 = x * 2^7,
                   Z9 = x * 2^8,
                   ZA = x * 2^9, 
                   ZB = x * 2^10,
                   ZC = x * 2^11,
                   ZD = x * 2^12,
                   ZE = x * 2^13,
                   ZF = x * 2^14,
                   ZG = x * 2^15,
                   ZH = x * 2^16)))

dtIOBJ[, G1AAGCMB:=rowSums(.SD)]

# for (col in 1:10) set(dtIOBJ, which(dtIOBJ[[col]] != 0), col, 1)
dtIOBJ <- dtIOBJ[, lapply(.SD, as.character)]
for (col in 1:17) set(dtIOBJ, which(dtIOBJ[[col]] != "0"), col, "Y")
for (col in 1:17) set(dtIOBJ, which(dtIOBJ[[col]] == "0"), col, "N")

setcolorder(dtIOBJ,  c("G1AAGCMB", setdiff(names(dtIOBJ), "G1AAGCMB")))

write.table(x= dtIOBJ, file= "C:/FTP/AAG_COMB.csv", quote = TRUE, 
            sep = ";", row.names = FALSE, col.names = TRUE)
