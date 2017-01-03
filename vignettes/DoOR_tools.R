## ---- echo = T, message = TRUE, results='hide'---------------------------
#load data
library(DoOR.functions)
library(DoOR.data)
load_door_data()

## ------------------------------------------------------------------------
recording <- data.frame(
  odorants = c(trans_id(c("BEDN", "ETAS"), "Code"), 
               trans_id("carbon dioxide", "Name")), 
  unit1 = c(.9,.1,.1), 
  unit2 = c(0, .1, 1)
)

## ---- fig.width=7.1, fig.height=4.5--------------------------------------
identify_sensillum(recording, base_size = 8)

## ---- fig.width=7.1, fig.height=4.5, fig.show='hide'---------------------
identify_sensillum(recording, min.cor = .99)

## ---- fig.width=7.1, fig.height=4.5--------------------------------------
identify_sensillum(recording, nshow = 5, base_size = 8)

## ---- fig.width=7.1, fig.height=4.5--------------------------------------
identify_sensillum(recording, sub = "ab", nshow = 5, base_size = 8)
identify_sensillum(recording, sub = c("ac","at"), nshow = 5, base_size = 8)

## ---- fig.width=7.1, fig.height=4.5--------------------------------------
identify_sensillum(recording, method = "dist", sub = "ab", nshow = 5, base_size = 8)

## ------------------------------------------------------------------------
sensillumX <- identify_sensillum(recording, method = "dist", sub = "ab", plot = F)
head(sensillumX)

## ------------------------------------------------------------------------
private_odorant("Or22a")

## ------------------------------------------------------------------------
private_odorant("Or22a", tag = "Name")

## ------------------------------------------------------------------------
private_odorant("Or22a", tag = "Name", sensillum = T)

## ------------------------------------------------------------------------
data <- data.frame(odorants  = Or22a$InChIKey, responses = Or22a$Hallem.2006.EN)
data <- na.omit(data)
head(data)
map_receptor(data = data, nshow = 5)

## ---- fig.width = 4.5, fig.height = 4.5----------------------------------
template <- data.frame(odorants  = Or22a$InChIKey, 
                       responses = Or22a$Hallem.2006.EN)

bp <- back_project(template, responding.unit = "Or22a")
plot(bp$back_projected$original.data, 
     bp$back_projected$back_projected.data, 
     xlab = "DoOR consensus response", 
     ylab = "back_projected data [spikes, Hallem.2006.EN]"
)

head(bp$back_projected)


## ------------------------------------------------------------------------
rm.SFRreset <- reset_sfr(response.matrix, "SFR")

sparse(x = rm.SFRreset[,"Or69a"], method = "ltk")
sparse(x = rm.SFRreset[,"Or69a"], method = "lts")

sparse(x = rm.SFRreset[,"Gr21a.Gr63a"], method = "ltk")
sparse(x = abs(rm.SFRreset[,"Gr21a.Gr63a"]), method = "lts")


