
library(tidyr)
install.packages("mclogit")
library(mclogit)

#Loading the data
Elephas<- read.csv("Male associations.csv")
head(Elephas)
Elephas$Group_type<- as.factor(Elephas$Group_type)


#Run all models
Mm0 <- mblogit(formula = Group_type ~ 1 , 
               random = ~ 1 | Habitat_type, 
               data = Elephas)
summary(Mm0)
print(Mm0)


Mm1 <- mblogit(formula = Group_type ~ Age , 
               random = ~ 1 | Habitat_type, 
               data = Elephas)
summary(Mm1)
print(Mm1)


Mm2 <- mblogit(formula = Group_type ~ Physiological_condition, 
               random = ~ 1 | Habitat_type, 
               data = Elephas)
summary(Mm2)
print(Mm2)


Mm3 <- mblogit(formula = Group_type ~ BCS, 
               random = ~ 1 | Habitat_type, 
               data = Elephas)
summary(Mm3)
print(Mm3)



Mm4 <- mblogit(formula = Group_type ~ Age + Physiological_condition, 
               random = ~ 1 | Habitat_type, 
               data = Elephas)
summary(Mm4)
print(Mm4)

Mm5 <- mblogit(formula = Group_type ~ Age + BCS, 
               random = ~ 1 | Habitat_type, 
               data = Elephas)
summary(Mm5)
print(Mm5)

Mm6 <- mblogit(formula = Group_type ~ BCS + Physiological_condition, 
               random = ~ 1 | Habitat_type, 
               data = Elephas)
summary(Mm6)
print(Mm6)


Mm7 <- mblogit(formula = Group_type ~ Age + BCS + Physiological_condition, 
               random = ~ 1 | Habitat_type, 
               data = Elephas)
summary(Mm7)
print(Mm7)


#Extracting AIC
aic_Mm0<- AIC(Mm0)
aic_Mm1<- AIC(Mm1)
aic_Mm2<- AIC(Mm2)
aic_Mm3<- AIC(Mm3)
aic_Mm4<- AIC(Mm4)
aic_Mm5<- AIC(Mm5)
aic_Mm6<- AIC(Mm6)
aic_Mm7<- AIC(Mm7)


#Data frame based on AIC of the models
model_results <- data.frame(
  Model = c("Mm0", "Mm1", "Mm2", "Mm3", "Mm4", "Mm5","Mm6", "Mm7"),
  AIC = c(aic_Mm0, aic_Mm1, aic_Mm2, aic_Mm3, aic_Mm4, aic_Mm5, aic_Mm6, aic_Mm7)
)

# delta AICc 
model_results$Delta_AIC <- model_results$AIC - min(model_results$AIC)
model_results$Delta_AIC

#  model weights
model_results$Weight <- exp(-0.5 * model_results$Delta_AIC) / 
  sum(exp(-0.5 * model_results$Delta_AIC))

#Model selection table
print(model_results)


# Variance Inflation Factor for best model Mm4
library(car)
vif_Mm4<- vif(Mm4)
vif_Mm4

#Obtaining odds ratios,95% Confidence interval and p values for each parameter
library(sjPlot)
sjPlot::tab_model(Mm4) 