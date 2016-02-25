## ---- echo = T, message = TRUE, results='hide'---------------------------
#load data
library(DoOR.functions)
library(DoOR.data)
loadData()

## ------------------------------------------------------------------------
recording <- data.frame(
  odorants = c(transID(c("BEDN", "ETAS"), "Code"), 
               transID("carbon dioxide", "Name")), 
  unit1 = c(.9,.1,.1), 
  unit2 = c(0, .1, 1)
)

## ---- fig.width=7.1, fig.height=4.5--------------------------------------
identifySensillum(recording, base_size = 8)

## ---- fig.width=7.1, fig.height=4.5, fig.show='hide'---------------------
identifySensillum(recording, min.cor = .99)

## ---- fig.width=7.1, fig.height=4.5--------------------------------------
identifySensillum(recording, nshow = 5, base_size = 8)

## ---- fig.width=7.1, fig.height=4.5--------------------------------------
identifySensillum(recording, sub = "ab", nshow = 5, base_size = 8)
identifySensillum(recording, sub = c("ac","at"), nshow = 5, base_size = 8)

## ---- fig.width=7.1, fig.height=4.5--------------------------------------
identifySensillum(recording, method = "dist", sub = "ab", nshow = 5, base_size = 8)

## ------------------------------------------------------------------------
sensillumX <- identifySensillum(recording, method = "dist", sub = "ab", plot = F)
head(sensillumX)

## ------------------------------------------------------------------------
privateOdorant("Or22a")

## ------------------------------------------------------------------------
privateOdorant("Or22a", tag = "Name")

## ------------------------------------------------------------------------
privateOdorant("Or22a", tag = "Name", sensillum = T)

## ------------------------------------------------------------------------
data <- data.frame(odorants  = Or22a$InChIKey, responses = Or22a$Hallem.2006.EN)
data <- na.omit(data)
head(data)
mapReceptor(data = data, nshow = 5)

## ---- fig.width = 4.5, fig.height = 4.5----------------------------------
template <- data.frame(odorants  = Or22a$InChIKey, 
                       responses = Or22a$Hallem.2006.EN)

bp <- backProject(template, responding.unit = "Or22a")
plot(bp$backprojected$original.data, 
     bp$backprojected$backprojected.data, 
     xlab = "DoOR consensus response", 
     ylab = "backprojected data [spikes, Hallem.2006.EN]"
)

head(bp$backprojected)


## ------------------------------------------------------------------------
rm.SFRreset <- resetSFR(response.matrix, "SFR")

sparse(x = rm.SFRreset[,"Or69a"], method = "ltk")
sparse(x = rm.SFRreset[,"Or69a"], method = "lts")

sparse(x = rm.SFRreset[,"Gr21a.Gr63a"], method = "ltk")
sparse(x = abs(rm.SFRreset[,"Gr21a.Gr63a"]), method = "lts")


