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
update_database("Or92a", permutation = F, plot = T)

## ---- fig.width = 7.1, fig.height = 5.5----------------------------------
update_database("Or67a", permutation = T, plot = F)

## ---- fig.width = 7.1, fig.height = 5.5----------------------------------
merge <- model_response(Or67a, plot = F)
knitr::kable(head(merge$model.response))

## ---- fig.width=5, fig.height=5.5----------------------------------------
SEQ <- c("Hallem.2006.EN","Kreher.2008.EN","Hallem.2006.EN")
merge <- model_response_seq(Or35a, SEQ = SEQ, plot = TRUE)
head(merge)

## ------------------------------------------------------------------------
remove_study(study = "Hallem.2004.EN")

## ------------------------------------------------------------------------
trans_id("123-92-2")
trans_id("123-92-2", to = "Name")
trans_id("carbon dioxide", from = "Name", to = "SMILES")

odorants <- c("carbon dioxide", "pentanoic acid", "water", "benzaldehyde", "isopentyl acetate")
trans_id(odorants, from = "Name", to = "InChI")


## ------------------------------------------------------------------------
rm_sfrReset <- reset_sfr(x = door_response_matrix, sfr = "SFR")
knitr::kable(rm_sfrReset[1:10,6:15], digits = 2)

## ------------------------------------------------------------------------
reset_sfr(x = c(1:10), sfr = 4)

## ------------------------------------------------------------------------
door_default_values("ident")
door_default_values("colors")

## ------------------------------------------------------------------------
odorants  <- trans_id(c("carbon dioxide", "isopentyl acetate"), from = "Name")
responses <- get_responses(odorants)
responses <- na.omit(responses)
knitr::kable(head(responses))

## ------------------------------------------------------------------------
odorants  <- trans_id(c("carbon dioxide", "isopentyl acetate"), from = "Name")
responses <- get_normalized_responses(odorants)
responses <- na.omit(responses)
knitr::kable(head(responses))

## ------------------------------------------------------------------------
counts <- countStudies()
knitr::kable(counts[1:10,6:15])

## ------------------------------------------------------------------------
# export_door_data(".csv")                  	# export all data as .csv files
# export_door_data(".txt", all.data = FALSE) 	# export odorant responses data only as .txt files

