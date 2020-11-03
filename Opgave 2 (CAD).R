#' ---
#' title: "Model search and Bayesian networks"
#' author: "SÃ¸ren HÃ¸jsgaard"
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output:
#'    html_document:
#'       toc: true
#' ---


library(gRain)
library(bnlearn)
library(magrittr)
data(cad1, package="gRbase")
names(cad1)

use <- c("Sex", "CAD", "Inherit", "Smoker", "Hyperchol", "Heartfail", "AMI")
dat1 <- cad1[, use]


## Start search from empty graph
mm1 <- hc(dat1)
mm1

## Start search from complete graph
sat <-random.graph(use, prob = 1)
mm2 <- hc(dat1, start=sat)
mm2

par(mfrow=c(1,2))
plot(mm1)
plot(mm2)

## Create Bayesian network
bn1 <- as.grain(bn.fit(mm1, dat1))
bn2 <- as.grain(bn.fit(mm2, dat1))

## Predict data

## Sample 40 random rows
set.seed(1213)
userow <- sample(nrow(dat1), 40)
wdat1 <- dat1[userow,]

pred1 <- predict(bn1, newdata=wdat1, response="CAD")
pred2 <- predict(bn2, newdata=wdat1, response="CAD")

table(wdat1$CAD, pred1$pred$CAD)
table(wdat1$CAD, pred2$pred$CAD)

## Procent 

table(wdat1$CAD, pred1$pred$CAD)/40*100
table(wdat1$CAD, pred2$pred$CAD)/40*100

#' Notice:
#' 
#' Prediction based on same data as we used for fitting / model search
#' is cheating. Use cad2 data instead.
#'
#' What are the misclassification errors under various models?
#'
#' Which misclassifications are the most serious ones?
#' 
#' Using cad2:

data(cad2, package="gRbase")
names(cad2)

use <- c("Sex", "CAD", "Inherit", "Smoker", "Hyperchol", "Heartfail", "AMI")
dat2 <- cad2[, use]

## Sample 40 random rows
set.seed(1213)
userow <- sample(nrow(dat2), 40)
wdat2 <- dat2[userow,]

pred3 <- predict(bn1, newdata=wdat2, response="CAD")
pred4 <- predict(bn2, newdata=wdat2, response="CAD")

table(wdat2$CAD, pred3$pred$CAD)
table(wdat2$CAD, pred4$pred$CAD)

## Procenter
table(wdat2$CAD, pred3$pred$CAD)/40*100
table(wdat2$CAD, pred4$pred$CAD)/40*100

#' Type 1 fejl: At forkaste, selvom patienten har det
#' Type 2 fejl: At acceptere, selvom patienten ikke har det. 
#' 
#' Det kan observeres at type 1 og 2 fejl er den samme for pred4 for cad2
#' Dog for pred3 for cad2, der er 15% af type 1 fejl og 10% af type 2 fejl 
#' 
#' Hvis behandlingen er voldsom krævende ville type 2 fejl være galt, dog kan man teste flere gange, for at undgå type 2 fejl
#' Type 1 fejl er "the most Severe", da patienten ikke vil få behandlingen i dette tilfælde
#' Dette kan medfører store koncekvenser for patienten



