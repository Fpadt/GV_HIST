dtCF701 <- dtCF701[, FKDAT:= as.Date(FKDAT, "%Y%m%d")]

```{r ggplot}
library(ggplot2)
library(data.table)
dtGG <- dtSAMPLE[, sum(SALES), by = .(FKDAT)]
dtGG[, WKDAY:= as.factor(format(FKDAT, "%w"))]
qplot(FKDAT, V1, data = dtGG, geom = "point", color = WKDAY)

dtGG <- dtGG[, WK:= format(FKDAT, "%Y%V")]
dtGG <- dtGG[, sum(V1), by = .(WK)]
setkey(dtGG, WK)
qplot(WK, V1, data = dtGG, geom = "point")

dtGG <- dtSAMPLE[, sum(SALES), 
                 by = .(MNTH, M, MATGRP_TLG, TGTGRP_TLG)]
setnames(dtGG, "V1", "SALES")
setkey(dtGG, MNTH)
# qplot(MNTH, V1, data = dtGG, geom = "point", color = M, size= M)
qplot(MNTH, SALES, data = dtGG, 
      geom = "point", facets = MATGRP_TLG ~ TGTGRP_TLG, color = M)

dtGG <- dtSAMPLE[, sum(SALES), 
                 by = .(MNTH)]
setnames(dtGG, "V1", "SALES")
setkey(dtGG, MNTH)
qplot(MNTH, SALES, data = dtGG, geom = "smooth")

```



dtSTORE <- dtCF701[, .N, by = .(FKDAT, WERKS)]
dtSTORE <- dtSTORE[, .N, by = .(FKDAT)]
dtSTORE <- dtSTORE[, FKDAT:= as.Date(FKDAT, "%Y%m%d")]
dtSTORE <- dtSTORE[, WKDAY:=format(FKDAT, "%a")]
qplot(FKDAT, N, data = dtSTORE, geom = "point", color = WKDAY)


dtMAT <- dtCF701[, .N, by = .(FKDAT, MATNR)]
dtMAT <- dtMAT[, .N, by = .(FKDAT)]
dtMAT <- dtMAT[, FKDAT:= as.Date(FKDAT, "%Y%m%d")]
dtMAT <- dtMAT[, WKDAY:=format(FKDAT, "%a")]
qplot(FKDAT, N, data = dtMAT, geom = "point", color = WKDAY)

### week
dtSTORE <- dtCF701[, .N, by = .(CALWK, WERKS)]
dtSTORE <- dtSTORE[, .N, by = .(CALWK)]
qplot(CALWK, N, data = dtSTORE, geom = "point")

dtMAT <- dtCF701[, .N, by = .(CALWK, MATNR)]
dtMAT <- dtMAT[, .N, by = .(CALWK)]
qplot(CALWK, N, data = dtMAT, geom = "point")

### month
dtMAT <- dtCF701[, .N, by = .(CALMTH, MATNR)]
dtMAT <- dtMAT[, .N, by = .(CALMTH)]
qplot(CALMTH, N, data = dtMAT, geom = "point")

dtSALES <- dtDATA[ , sum(SALES), by = .(CALMTH)]

x <- ts(dtSALES$V1, start = c(2006,1), frequency=12)
plot(stl(x, s.window = "periodic"))
seasonplot(x)

dtPLOT <- dtSAMPLE[ , sum(SALES), by = .(CALMTH)]
x <- ts(dtPLOT$V1, start = c(2006,1), frequency=12)
seasonplot(x)

dtSAMP <- dtSUNG[GENDER == "M" & PRCBND == "GO"]
dtSAMP <- dtSAMP[ , sum(SALES), by = .(CALMTH, MATNR)]
