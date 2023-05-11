library(modelr)
library(magrittr)
library(ggplot2)
library(tidyverse)
library(dplyr)
PretImobiliare<-Problema2%>% 
  mutate(Age = YrSold - YearBuilt)
PretImobiliare$GarageFinish<-dplyr::recode(PretImobiliare$GarageFinish, .missing = "Missing")
View(PretImobiliare)
g1<-ggplot(data=PretImobiliare, aes(x=GrLivArea, y=SalePrice)) #creare spatiu pt grafic
g2<-g1+ geom_point()
g2
ggplot(data=PretImobiliare, aes(x=Age, y=SalePrice)) + geom_point()
model1 <- lm(SalePrice ~ GrLivArea, data = PretImobiliare)
model1
model1$coefficients
model1$coefficients[1]
model1$coefficients[2]
g2 + geom_abline(intercept = model1$coefficients[1], slope = model1$coefficients[2])
PretImobiliare <- PretImobiliare %>% 
add_predictions(model1, var = "Predictii_model1") %>%
add_residuals(model1, var = "Reziduuri_model1")
mape(model1, PretImobiliare) 
rmse(model1, PretImobiliare
model_matrix(data = PretImobiliare, SalePrice ~ GrLivArea)
model_matrix(data = PretImobiliare, SalePrice ~ GrLivArea - 1)
model2<- lm(SalePrice ~ GrLivArea + LotArea + TotRmsAbvGrd + Age, data = PretImobiliare)
mape(model2, PretImobiliare) #mai mica decat in cazul modelului 1 (cu un singur predictor)
rmse(model2, PretImobiliare)
levels(PretImobiliare$GarageFinish)
PretImobiliare$GarageFinish<-factor(PretImobiliare$GarageFinish, levels=c("Missing","Unf","RFn", "Fin"), ordered=TRUE)
Mediecond1<-tapply(PretImobiliare$SalePrice, INDEX=PretImobiliare$GarageFinish, FUN = mean)
Mediecond1 

mediecond2<-tapply(PretImobiliare$SalePrice, INDEX=PretImobiliare$OverallCond, FUN = mean)
mediecond2
levels(PretImobiliare$OverallCond)
PretImobiliare$OverallCond<-factor(PretImobiliare$OverallCond)

table(PretImobiliare$GarageFinish)
ggplot(PretImobiliare) + geom_boxplot(aes(GarageFinish, SalePrice))
PretImobiliare$GarageFinish<-as.character(PretImobiliare$GarageFinish)
PretImobiliare$GarageFinish<-factor(PretImobiliare$GarageFinish) 
PretImobiliare$OverallCond<-as.character(PretImobiliare$OverallCond) 
PretImobiliare$OverallCond<-factor(PretImobiliare$OverallCond)
model3<- lm(SalePrice ~ factor(GarageFinish), data = PretImobiliare)
model3
m<-model.matrix(model3) #matricea cu valorile variabilelor explicative X
head(m) 
PretImobiliare$GarageFinish<-factor(PretImobiliare$GarageFinish)
levels(PretImobiliare$GarageFinish) #sunt sortate alfabetic categ 1 este de referinţă (Fin; pt ea nu este inclusa variabilă dummy in regresie).
PretImobiliare$GarageFinish<-relevel(PretImobiliare$GarageFinish, ref = "Missing") 
model3<- lm(SalePrice ~ factor(GarageFinish), data = PretImobiliare)
model3
mape(model3, PretImobiliare)
rmse(model3, PretImobiliare)
PretImobiliare <- PretImobiliare %>% 
  add_predictions(model3, var = "Predictii_model3") %>% 
  add_residuals(model3, var = "Reziduuri_model3")

PretImobiliare$OverallCond<-factor(PretImobiliare$OverallCond)
levels(PretImobiliare$OverallCond)
model4<- lm(SalePrice ~ GrLivArea + LotArea + TotRmsAbvGrd + Age + factor(OverallCond) + factor(GarageFinish), data = PretImobiliare)
model4
mape(model4, PretImobiliare)
rmse(model4, PretImobiliare)
PretImobiliare <- PretImobiliare %>% 
  add_predictions(model4, var = "Predictii_model4") %>%
  add_residuals(model4, var = "Reziduuri_model4")
summary(model4) 
confint(model4)

model5<- lm(SalePrice ~ LotArea + TotRmsAbvGrd + Age + factor(OverallCond) + factor(GarageFinish), data = PretImobiliare)
summary(model5)
mape(model5, PretImobiliare)
rmse(model5, PretImobiliare)
model6<- lm(SalePrice ~ GrLivArea + LotArea + Age + factor(OverallCond) + factor(GarageFinish), data = PretImobiliare)
summary(model6)
mape(model6, PretImobiliare)
rmse(model6, PretImobiliare)
PretImobiliare <- PretImobiliare %>% 
  add_predictions(model6, var = "Predictii_model6") %>%
  add_residuals(model6, var = "Reziduuri_model6")
ggplot(PretImobiliare, aes(GrLivArea, Reziduuri_model6)) + geom_point()
ggplot(PretImobiliare, aes(GrLivArea, Predictii_model6)) + geom_point()
ggplot(PretImobiliare, aes(GarageFinish, Predictii_model6)) + geom_point()
ggplot(PretImobiliare, aes(GarageFinish, Reziduuri_model6)) + geom_point()
ggplot(PretImobiliare, aes(GarageFinish, Reziduuri_model6)) + geom_boxplot()
hist(PretImobiliare$SalePrice)
max(PretImobiliare$SalePrice)
PretImobiliare %>%
  count(GarageFinish)
ggplot(data = PretImobiliare) +
  geom_bar(aes(x = GarageFinish))
PretImobiliare %>% 
  count(cut_width(SalePrice, 40000)) #lungimea intervalelor 40000
ggplot(data = PretImobiliare) +
  geom_histogram(aes(x = SalePrice))
ggplot(data = PretImobiliare) +
  geom_histogram(aes(x = SalePrice), binwidth=40000)
PretImobiliare %>% summary()
ggplot(data = PretImobiliare, aes(x = SalePrice, colour = GarageFinish)) +
  geom_freqpoly()	#poligonul frecventelor pentru preţ pe categorii de finisare garaj

ggplot(data = PretImobiliare, aes(x = SalePrice, colour = GarageFinish)) +
  geom_boxplot()
ggplot(data = PretImobiliare) +
  geom_boxplot(aes(x = reorder(GarageFinish, SalePrice, FUN = median), y = SalePrice)) 

ggplot(data = PretImobiliare) +
  geom_boxplot(aes(x = reorder(GarageFinish, SalePrice, FUN = mean), y = SalePrice))

group_by(PretImobiliare, GarageFinish) %>%
  summarise(
    mean = mean(SalePrice, na.rm = TRUE),
    sd = sd(SalePrice, na.rm = TRUE)
  ) 
anova<-aov(formula = SalePrice ~ GarageFinish, data = PretImobiliare) 
summary(anova) 
oneway.test(SalePrice ~ GarageFinish, data = PretImobiliare)
ggplot(data = PretImobiliare, aes(x = SalePrice, colour = OverallCond)) +
  geom_boxplot()

PretImobiliare$OverallCond<-factor(PretImobiliare$OverallCond, ordered=TRUE)
ggplot(data = PretImobiliare, aes(x = SalePrice, colour = OverallCond)) +
  geom_boxplot() 

ggplot(data = PretImobiliare) +
  geom_boxplot(aes(x = reorder(OverallCond, SalePrice, FUN = median), y = SalePrice))

group_by(PretImobiliare, OverallCond) %>%
  summarise(
    media = mean(SalePrice, na.rm = TRUE),
    sd = sd(SalePrice, na.rm = TRUE),
    mediana = median(SalePrice, na.rm = TRUE)
  )
PretImobiliare %>% 
  count(OverallCond)

anova<-aov(formula = SalePrice ~ OverallCond, data = PretImobiliare)
summary(anova)
ggplot(data = PretImobiliare) +
  geom_count(aes(x = MSZoning, y = OverallCond))
xtabs(formula = ~ OverallCond + MSZoning, data = PretImobiliare) #numărul de observaţii pentru fiecare combinaţie
#sau
PretImobiliare %>% 
  count(OverallCond, MSZoning) %>%  
  ggplot(aes(x = MSZoning, y = OverallCond)) +
  geom_tile(aes(fill = n))
install.packages("vcd")
library(vcd)
ass<-assocstats(table(PretImobiliare$OverallCond, PretImobiliare$MSZoning))
summary(ass)

mosaic(~ OverallCond + MSZoning,
       data = PretImobiliare, shade = TRUE)
ggplot(data=PretImobiliare, aes(x=GrLivArea, y=SalePrice)) + geom_point()
ggplot(data = PretImobiliare) +
  geom_bin2d(aes(x = GrLivArea, y = SalePrice))
cor() # coeficientul de corelaţie liniară
cor(PretImobiliare$GrLivArea, PretImobiliare$SalePrice)
install.packages("ggstatsplot")
library(ggstatsplot)
ggscatterstats(
  data = PretImobiliare,
  x = GrLivArea,
  y = SalePrice
)

