##### This script tests for the correct use of ghp #####

## Libraries
library("vibe")
library("testthat")
library("titanic")

## Remove everything
rm(list = ls())

## Data
titanic <- titanic::titanic_train
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Sex <- as.numeric(as.factor(titanic$Sex))
### --- Fitting the models --- ###

## GLM
glm_bin <- glm(formula = Survived ~ Age + Sex + SibSp + Fare + Parch,
               data = titanic, family = "binomial")

### --- Trying vibe --- ###
