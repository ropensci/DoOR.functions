## ---- results='hide'-----------------------------------------------------
library(DoOR.data)
library(DoOR.functions)
load_door_data()

## ---- echo=FALSE---------------------------------------------------------
tmp <- Or22a[c(1,3:5), c(3,6)]
colnames(tmp)[2] <- "Or22a"
knitr::kable(tmp)

## ---- echo=FALSE---------------------------------------------------------
tmp <- Or22a[c(1,3:5), c(1:6)]
colnames(tmp)[6] <- "Or22a"
knitr::kable(tmp)

## ---- fig.width = 7.1, fig.height = 5.5----------------------------------
updateDatabase("Or92a", permutation = F, plot = T)

## ---- fig.width = 7.1, fig.height = 5.5----------------------------------
updateDatabase("Or67a", permutation = T, plot = F)

## ---- fig.width = 7.1, fig.height = 5.5----------------------------------
merge <- modelRP(Or67a, plot = F)
knitr::kable(head(merge$model.response))

## ---- fig.width=5, fig.height=5.5----------------------------------------
SEQ <- c("Hallem.2006.EN","Kreher.2008.EN","Hallem.2006.EN")
merge <- modelRPSEQ(Or35a, SEQ = SEQ, plot = TRUE)
head(merge)

## ------------------------------------------------------------------------
removeStudy(study = "Hallem.2004.EN")

## ------------------------------------------------------------------------
transID("123-92-2")
transID("123-92-2", to = "Name")
transID("carbon dioxide", from = "Name", to = "SMILES")

odorants <- c("carbon dioxide", "pentanoic acid", "water", "benzaldehyde", "isopentyl acetate")
transID(odorants, from = "Name", to = "InChI")


## ------------------------------------------------------------------------
rm_sfrReset <- resetSFR(x = response.matrix, sfr = "SFR")
knitr::kable(rm_sfrReset[1:10,6:15], digits = 2)

## ------------------------------------------------------------------------
resetSFR(x = c(1:10), sfr = 4)

## ------------------------------------------------------------------------
default.val("ident")
default.val("colors")

## ------------------------------------------------------------------------
odorants  <- transID(c("carbon dioxide", "isopentyl acetate"), from = "Name")
responses <- getResponses(odorants)
responses <- na.omit(responses)
knitr::kable(head(responses))

## ------------------------------------------------------------------------
odorants  <- transID(c("carbon dioxide", "isopentyl acetate"), from = "Name")
responses <- getNormalizedResponses(odorants)
responses <- na.omit(responses)
knitr::kable(head(responses))

## ------------------------------------------------------------------------
counts <- countStudies()
knitr::kable(counts[1:10,6:15])

## ------------------------------------------------------------------------
# exportData(".csv")                  	# export all data as .csv files
# exportData(".txt", all.data = FALSE) 	# export odorant responses data only as .txt files

