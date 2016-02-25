## ---- echo = T, message = TRUE, results='hide'---------------------------
#load data
library(DoOR.functions)
library(DoOR.data)
loadData()

## ----fig.width=7.1, fig.height=4-----------------------------------------
dplot_responseMatrix(response.matrix[2:50,], tag = "Name", base_size = 8)

## ----fig.width=7.1, fig.height=4-----------------------------------------
dplot_responseMatrix(resetSFR(response.matrix, "SFR")[2:50,], tag = "Name", base_size = 8)

## ---- fig.width=7.1------------------------------------------------------
dplot_ALmap("QSJXEFYPDANLFS-UHFFFAOYSA-N", base_size = 8)

## ---- fig.width=7.1, warning=FALSE---------------------------------------
dplot_ALmap(transID("benzaldehyde", from = "Name"), tag = "receptor", main = "SMILES", base_size = 8)

## ---- fig.width=7.1, fig.height=2----------------------------------------
dplot_ALmap(transID("628-63-7", from = "CAS"), tag = "", main = "", legend = F, scalebar = F)

## ------------------------------------------------------------------------
dplot_tuningCurve(receptor = "Or22a", base_size = 8)

## ------------------------------------------------------------------------
dplot_tuningCurve(receptor = "Or22a", zero = "", base_size = 8)

## ------------------------------------------------------------------------
dplot_tuningCurve(receptor = "receptor X", response.vector = c(1:100), base_size = 8)

## ------------------------------------------------------------------------
dplot_tuningCurve(odorant = "PGMYKACGEOXYJE-UHFFFAOYSA-N", base_size = 8)

## ------------------------------------------------------------------------
dplot_tuningCurve(odorant = "PGMYKACGEOXYJE-UHFFFAOYSA-N", odor.main = "SMILES", base_size = 8)
dplot_tuningCurve(odorant = "CURLTUGMZLYLDI-UHFFFAOYSA-N", odor.main = "InChI", base_size = 8)

## ------------------------------------------------------------------------
dplot_tuningCurve(odorant = transID("carbon dioxide", from = "Name"), fill.odorant = "#FF0000", base_size = 8)

## ------------------------------------------------------------------------
library(ggplot2)
dplot_tuningCurve(odorant = transID("carbon dioxide", from = "Name"), base_size = 8) +
  theme(panel.background = element_rect(fill = "grey", color = "magenta"))

## ---- fig.width=5, fig.height=6------------------------------------------
dplot_responseProfile("Gr21a.Gr63a", tag = "Name", base_size = 8)

## ---- fig.width=5, fig.height=6------------------------------------------
dplot_responseProfile("Gr21a.Gr63a", tag = "Name", base_size = 8, zero ="")

## ---- fig.width=5, fig.height=6------------------------------------------
dplot_responseProfile("Gr21a.Gr63a", tag = "CAS", base_size = 8, colored = F)

## ------------------------------------------------------------------------
dplot_compareProfiles(x = Or22a, y = Or22a, by.x = "Pelz.2006.AntEC50",
                         by.y = "Hallem.2004.EN", tag = "Name", base_size = 8)

## ---- fig.width = 7.1----------------------------------------------------
dplot_compareProfiles(x = response.matrix, by.x = "Or35a", by.y = "ac3B",
                         tag = "Name", base_size = 8)

## ---- fig.width = 7.1----------------------------------------------------
dplot_compareProfiles(x = resetSFR(response.matrix, "SFR"), by.x = "Or35a", by.y = "ac3B",
                         tag = "Name", base_size = 8)

## ---- fig.width = 6, fig.height = 6--------------------------------------
odors <-
  transID(c("pentyl acetate", "carbon dioxide", "2,3-butanedione"), from = "Name")
dplot_acrossReceptors(odors, tag = "Name", base_size = 8)

## ---- fig.width = 6, fig.height = 6--------------------------------------
dplot_acrossOSNs(odors, base_size = 8, plot.type = 2)

## ---- fig.width = 7.1, fig.height = 5------------------------------------
dplot_acrossOSNs(odors, base_size = 8, plot.type = 1)

## ---- fig.width = 7.1, fig.height = 5------------------------------------
dplot_acrossOSNs(odors, base_size = 8, plot.type = 1, sub = "ab")

## ---- fig.width = 7.1, fig.height = 5------------------------------------
dplot_acrossOSNs(odors, base_size = 8, plot.type = 1, sub = c("ac", "at"))

