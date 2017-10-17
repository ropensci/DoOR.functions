## ---- echo = TRUE, message = TRUE, results='hide'---------------------------
#load data
library(DoOR.functions)
library(DoOR.data)
load_door_data(nointeraction = TRUE)

## ----fig.width=7.1, fig.height=4-----------------------------------------
dplot_response_matrix(door_response_matrix[2:50,], tag = "Name", base_size = 8)

## ----fig.width=7.1, fig.height=4-----------------------------------------
dplot_response_matrix(reset_sfr(door_response_matrix, "SFR")[2:50,], tag = "Name", base_size = 8)

## ---- fig.width=7.1------------------------------------------------------
dplot_al_map("QSJXEFYPDANLFS-UHFFFAOYSA-N", base_size = 8)

## ---- fig.width=7.1, warning=FALSE---------------------------------------
dplot_al_map(trans_id("benzaldehyde", from = "Name"), tag = "receptor", main = "SMILES", base_size = 8)

## ---- fig.width=7.1, fig.height=2----------------------------------------
dplot_al_map(trans_id("628-63-7", from = "CAS"), tag = "", main = "", legend = FALSE, scalebar = FALSE)

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
dplot_tuningCurve(odorant = trans_id("carbon dioxide", from = "Name"), fill.odorant = "#FF0000", base_size = 8)

## ------------------------------------------------------------------------
library(ggplot2)
dplot_tuningCurve(odorant = trans_id("carbon dioxide", from = "Name"), base_size = 8) +
  theme(panel.background = element_rect(fill = "grey", color = "magenta"))

## ---- fig.width=5, fig.height=6------------------------------------------
dplot_response_profile("Gr21a.Gr63a", tag = "Name", base_size = 8)

## ---- fig.width=5, fig.height=6------------------------------------------
dplot_response_profile("Gr21a.Gr63a", tag = "Name", base_size = 8, zero ="")

## ---- fig.width=5, fig.height=6------------------------------------------
dplot_response_profile("Gr21a.Gr63a", tag = "CAS", base_size = 8, colored = FALSE)

## ------------------------------------------------------------------------
dplot_compare_profiles(x = Or22a, y = Or22a, by.x = "Pelz.2006.AntEC50",
                         by.y = "Hallem.2004.EN", tag = "Name", base_size = 8)

## ---- fig.width = 7.1----------------------------------------------------
dplot_compare_profiles(x = door_response_matrix, by.x = "Or35a", by.y = "ac3B",
                         tag = "Name", base_size = 8)

## ---- fig.width = 7.1----------------------------------------------------
dplot_compare_profiles(x = reset_sfr(door_response_matrix, "SFR"), by.x = "Or35a", by.y = "ac3B",
                         tag = "Name", base_size = 8)

## ---- fig.width = 6, fig.height = 6--------------------------------------
odors <-
  trans_id(c("pentyl acetate", "carbon dioxide", "2,3-butanedione"), from = "Name")
dplot_across_ru(odors, tag = "Name", base_size = 8)

## ---- fig.width = 6, fig.height = 6--------------------------------------
dplot_across_osns(odors, base_size = 8, plot.type = 2)

## ---- fig.width = 7.1, fig.height = 5------------------------------------
dplot_across_osns(odors, base_size = 8, plot.type = 1)

## ---- fig.width = 7.1, fig.height = 5------------------------------------
dplot_across_osns(odors, base_size = 8, plot.type = 1, sub = "ab")

## ---- fig.width = 7.1, fig.height = 5------------------------------------
dplot_across_osns(odors, base_size = 8, plot.type = 1, sub = c("ac", "at"))
