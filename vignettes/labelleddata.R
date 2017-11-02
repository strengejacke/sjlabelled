## ----echo = FALSE--------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
factor(c("low", "high", "mid", "high", "low"))

## ------------------------------------------------------------------------
library(sjlabelled)
data(efc)
str(efc$e42dep)

## ----warning=FALSE, fig.height=6, fig.width=7----------------------------
library(sjlabelled)
data(efc)
barplot(
  table(efc$e42dep, efc$e16sex), 
  beside = T, 
  legend.text = T
)

## ----warning=FALSE, fig.height=6, fig.width=7----------------------------
barplot(
  table(as_label(efc$e42dep),
        as_label(efc$e16sex)), 
  beside = T, 
  legend.text = T
)

