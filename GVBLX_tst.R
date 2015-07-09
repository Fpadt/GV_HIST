dtGVBLX     <- fread(file.path(".", "RAW_DATA", "GVBLX_HIST.txt"),
View(dtGVBLX[invoice_number == "39"][order(invoice_date)])

file = file.path("c:", "FTP", "GVBNL.csv")
dtGVBNL     <- fread(file.path("c:", "FTP", "GVBNL.csv"),  
                    sep = ",") 

View(dtGVBNL[BILL_NUM == "H0000000039"][order(SLSDATE)])
View(dtGVBNL[BILL_NUM == "H0000000039" & PLANT == "0700" ])


file = file.path("c:", "FTP", "POS_HISTORY.csv")
dtPOS     <- fread(file.path("c:", "FTP", "POS_HISTORY.csv"),  
                   sep = ",") 

View(dtPOS[BILL_NUM == "H0000000039"][order(SLSDATE)])
View(dtPOS[BILL_NUM == "H0000000039" & PLANT == "0700" ])

dtTST <- dtDATA[, .N, 
                by=.(BILL_NUM, BILL_ITEM, SLSDATE, MATERIAL, 
                     PLANT, G1LGART, G1LGSTNO)]
View(dtTST[BILL_NUM == "H0000000039"][order(PLANT, SLSDATE, BILL_ITEM)])


View(dtDATA[BILL_NUM == "H0000000039"][order(SLSDATE)])
View(dtDATA[ PLANT == "0700"])
0700