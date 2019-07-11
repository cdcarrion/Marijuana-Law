library(fitdistrplus)
library(dplyr)
library(tidyr)
library(plyr)
library(lme4)
library(stargazer)
library(tidyverse)
library(tibble)
library(ggplot2)
dat  <-  read.table(Data_EVS, header=FALSE) # poster's data
gamma2  <-  Data_EVS$year
descdist(gamma2)


data <- getURL("https://raw.githubusercontent.com/svmiller/wvs-usa-abortion-attitudes/master/wvs-usa-abortion-attitudes-data.csv")
Data <- read.csv(text = data) %>% tbl_df()

library(panelView)

ranef_sd <- function(model, grp){
  vc <- as.data.frame(lme4::VarCorr(model))
  result <- vc$sdcor[ vc$grp == grp]
  result <- sprintf("%.3f", round(result, 3))
  return(result)
}


crime_t <- read.csv("C:/Users/Usuario/Documents/Doc español/Marihuana_law/data/crime_t.csv", 
                  stringsAsFactors = T, 
                  sep = ";")  #DROP COLUMBIA ---- 51

crime <- read.csv("C:/Users/Usuario/Documents/Doc español/Marihuana_law/data/crime_4.csv", 
                  stringsAsFactors = T, 
                  sep = ";")  #DROP COLUMBIA ---- 51

#crime_numerated <- read.csv("C:/Users/Usuario/Documents/Doc español/Marihuana_law/data/crime_4.csv", 
#                            stringsAsFactors = T, 
#                            sep = ";")  



employ <- read.csv("C:/Users/Usuario/Documents/Doc español/Marihuana_law/data/emply_promd_csv2.csv", 
                   stringsAsFactors = T, 
                   sep = ";")
employ <- employ[c(1,2,4)]
names(employ)[names(employ) == "Series.ID"] <- "State"
names(employ)[names(employ) == "Value"] <- "Employ"


######
#levels(employ$Series.ID) <- c("Alabama",
#                    "Alaska",
#                    "Arizona",
#                    "Arkansas",
#                    "California",
#                    "Colorado",
#                    "Connecticut",
#                    "Delaware",
#                    "Florida",
#                    "Georgia",
#                    "Hawaii",
#                    "Idaho",
#                    "Illinois",
#                    "Indiana",
#                    "Iowa",
#                    "Kansas",
#                    "Kentucky",
#                    "Louisiana",
#                    "Maine",
#                    "Maryland",
#                    "Massachusetts",
#                    "Michigan",
#                    "Minnesota", 
#                    "Mississippi",
#                    "Missouri",
#                    "Montana",
#                    "Nebraska",
#                    "Nevada",
#                    "New Hampshire",
#                    "New Jersey",
#                    "New Mexico",
#                    "New York",
#                    "North Carolina",
#                    "North Dakota",
#                    "Ohio", 
#                    "Oklahoma",
#                    "Oregon",
#                    "Pennsylvania",
#                    "Rhode Island",
#                    "South Carolina",
#                    "South Dakota",
#                    "Tennessee",
#                    "Texas",
#                    "Utah",
#                    "Vermont",
#                    "Virginia",
#                    "Washington",
#                    "West Virginia",
#                    "Wisconsin",
#                    "Wyoming")

#####                    

povert <- read.csv("C:/Users/Usuario/Documents/Doc español/Marihuana_law/data/poverty.csv", 
                   stringsAsFactors = T, 
                   sep = ",")


povert <- povert[c(1,4,10)]
names(povert)[names(povert) == "State...County.Name"] <- "State"
names(povert)[names(povert) == "All.Ages.in.Poverty.Percent"] <- "povert_rate"
write.csv(povert,"C:\\Users\\Usuario\\Documents\\Doc español\\Marihuana_law\\data\\povert_r.csv", 
          row.names = FALSE)



unempl <- readxl::read_xls("C:/Users/Usuario/Documents/Doc español/Marihuana_law/data/Unemployment_for.xls")
#unempl <- read.csv("C:/Users/Usuario/Documents/Doc español/Marihuana_law/data/Unemployment_stat_cvs.csv", 
#                   stringsAsFactors = T, 
#                   sep = ";")

unempl <- gather(unempl, "year", "value", "1980":"2015")
names(unempl)[names(unempl) == "Area"] <- "State"
names(unempl)[names(unempl) == "value"] <- "unempl_rate"
names(unempl)[names(unempl) == "year"] <- "Year"

alc <- readxl::read_xls("C:/Users/Usuario/Documents/Doc español/Marihuana_law/data/alchool.xls", 
                sheet= 1, 
                skip= 127)  ## no se puede separar en csv  --- y tiene que ser del 97 al 2003 excel
alc <- select(alc, Year, State, Type, Gallons.of.ethanol) %>%
        filter(Type == "Beer")

alc$Gallons.of.ethanol <- alc$Gallons.of.ethanol/100000




proj <- join(crime, povert, by = c("Year", "State"), type="left", match="first")
proj <- join(proj, employ, by = c("Year", "State"), type="left", match="first")
proj <- join(proj, unempl, by = c("Year", "State"), type="left", match="first")
proj <- join(proj, alc, by = c("Year", "State"), type="left", match="first")

proj <- filter(proj, Year >= 1980 & Year <= 2014)

###### 
proj$Dummy <- if_else(proj$State == "Alaska" & proj$Year >= 1998,
                      1, if_else(proj$State == "Arizona" & proj$Year >= 2011,
                                 1, if_else(proj$State == "California" & proj$Year >= 1996,
                                            1, if_else(proj$State == "Colorado" & proj$Year >= 2001,
                                                       1, if_else(proj$State == "Connecticut" & proj$Year >= 2012,
                                                                  1, if_else(proj$State == "Delaware" & proj$Year >= 2011,
                                                                             1, if_else(proj$State == "Hawaii" & proj$Year >= 2001,
                                                                                        1, if_else(proj$State == "Illinois" & proj$Year >= 2013,
                                                                                                   1, if_else(proj$State == "Maine" & proj$Year >= 1999,
                                                                                                              1, if_else(proj$State == "Maryland" & proj$Year >= 2014,
                                                                                                                         1, if_else(proj$State == "Massachusetts" & proj$Year >= 2013,
                                                                                                                                    1, if_else(proj$State == "Michigan" & proj$Year >= 2008,
                                                                                                                                               1, if_else(proj$State == "Minnesota" & proj$Year >= 2014,
                                                                                                                                                          1, if_else(proj$State == "Montana" & proj$Year >= 2004,
                                                                                                                                                                     1, if_else(proj$State == "Nevada" & proj$Year >= 2001,
                                                                                                                                                                                1, if_else(proj$State == "New Hampshire" & proj$Year >= 2013,
                                                                                                                                                                                           1, if_else(proj$State == "New Jersey" & proj$Year >= 2010,
                                                                                                                                                                                                      1, if_else(proj$State == "New Mexico" & proj$Year >= 2007,
                                                                                                                                                                                                                 1, if_else(proj$State == "New York" & proj$Year >= 2014,
                                                                                                                                                                                                                            1, if_else(proj$State == "Oregon" & proj$Year >= 1998,
                                                                                                                                                                                                                                       1, if_else(proj$State == "Rhode Island" & proj$Year >= 2005,
                                                                                                                                                                                                                                                  1, if_else(proj$State == "Vermont" & proj$Year >= 2004,
                                                                                                                                                                                                                                                             1, if_else(proj$State == "Washington" & proj$Year >= 1998,
                                                                                                                                                                                                                                                                        1,0)))))))))))))))))))))))
##############
proj$C_D <- if_else(proj$State == "Alaska",
                                  1, if_else(proj$State == "Arizona",
                                             1, if_else(proj$State == "California",
                                                        1, if_else(proj$State == "Colorado",
                                                                   1, if_else(proj$State == "Connecticut",
                                                                              1, if_else(proj$State == "Delaware",
                                                                                         1, if_else(proj$State == "Hawaii",
                                                                                                    1, if_else(proj$State == "Illinois",
                                                                                                               1, if_else(proj$State == "Maine",
                                                                                                                          1, if_else(proj$State == "Maryland",
                                                                                                                                     1, if_else(proj$State == "Massachusetts",
                                                                                                                                                1, if_else(proj$State == "Michigan",
                                                                                                                                                           1, if_else(proj$State == "Minnesota",
                                                                                                                                                                      1, if_else(proj$State == "Montana",
                                                                                                                                                                                 1, if_else(proj$State == "Nevada",
                                                                                                                                                                                            1, if_else(proj$State == "New Hampshire",
                                                                                                                                                                                                       1, if_else(proj$State == "New Jersey",
                                                                                                                                                                                                                  1, if_else(proj$State == "New Mexico",
                                                                                                                                                                                                                             1, if_else(proj$State == "New York",
                                                                                                                                                                                                                                        1, if_else(proj$State == "Oregon",
                                                                                                                                                                                                                                                   1, if_else(proj$State == "Rhode Island",
                                                                                                                                                                                                                                                              1, if_else(proj$State == "Vermont",
                                                                                                                                                                                                                                                                         1, if_else(proj$State == "Washington",
                                                                                                                                                                                                                                                                                    1,0)))))))))))))))))))))))


proj$D_D <- proj$Dummy*proj$C_D
###### sdsdds
write.csv(proj,"C:\\Users\\Usuario\\Documents\\Doc español\\Marihuana_law\\data\\Data_final.csv", 
          row.names = FALSE)

proj$Employ <- proj$Employ/100000

panelView(Robbery ~ Dummy, 
          data = proj, 
          index = c("State","Year"), 
          xlab = "Year", 
          ylab = "State",
          pre.post = TRUE,
          by.timing = TRUE)


############

theme_steve <- function() {
  theme_bw() +
    theme(panel.border = element_blank(), 
          plot.caption=element_text(hjust=1, size=9,
                                    margin=margin(t=10),
                                    face="italic"),
          plot.title=element_text(hjust=0, size=18,
                                  margin=margin(b=10),
                                  face="bold"),
          axis.title.y=element_text(size=12,hjust=1,
                                    face="italic"),
          axis.title.x=element_text(hjust=1, size=12, face="italic"))
  
}

m_plot <- crime_t 
#  filter(Year >= 1990) %>%
mplot <-  dplyr::select(crime_t, Year, 
         Violent.crime.total,
         Robbery, 
         Aggravated.assault) %>%
  gather(key = "Variable", value = "Observaciones", -Year)
head(m_plot)


ggplot(m_plot, aes(x = Year, y = Observaciones)) + 
  geom_line(aes(color = Variable, linetype = Variable)) + 
  geom_vline(aes(xintercept = 1998, linetype = "dotted"))+
  geom_vline(aes(xintercept = 1999, linetype = "dotted"))+
  geom_vline(aes(xintercept = 2000, linetype = "dotted"))+
  geom_vline(aes(xintercept = 2001, linetype = "dotted"))+
  geom_vline(aes(xintercept = 2004, linetype = "dotted"))+
  geom_vline(aes(xintercept = 2005, linetype = "dotted"))+
  geom_vline(aes(xintercept = 2007, linetype = "dotted"))+
  geom_vline(aes(xintercept = 2008, linetype = "dotted"))+
  geom_vline(aes(xintercept = 2010, linetype = "dotted"))+
  geom_vline(aes(xintercept = 2011, linetype = "dotted"))+
  geom_vline(aes(xintercept = 2012, linetype = "dotted"))+
  geom_vline(aes(xintercept = 2013, linetype = "dotted"))+
  geom_vline(aes(xintercept = 2014, linetype = "dotted"))+
  annotate("text", x=1998, y = 1500000, 
           label = "Alaska \n1998",  family="Open Sans") +
  annotate("text", x=1998, y = 1700000, 
           label = "Oregon \n1998'",  family="Open Sans") +
  annotate("text", x=2000, y = 1000000, 
           label = "Hawaii \n2000'",  family="Open Sans") +
  annotate("text", x=2004, y = 800000, 
           label = "Montana \n2004'",  family="Open Sans") +
  annotate("text", x=2008, y = 1300000, 
           label = "Michigan \n2008'",  family="Open Sans") +
  annotate("text", x=2014, y = 1200000, 
           label = "Maryland \n2014'",  family="Open Sans") +
  annotate("text", x=2014, y = 1000000, 
           label = "New York \n2014'",  family="Open Sans") + theme_steve() 
###########


m1 <- lmer(data = proj, 
           log(Murder.and.nonnegligent.Manslaughter) ~  povert_rate + Employ + unempl_rate + Gallons.of.ethanol + Dummy + (1|Year) + (1|State))
m1.n <- lmer(data = proj, 
           log(Murder.and.nonnegligent.Manslaughter) ~  povert_rate + Employ + unempl_rate + Gallons.of.ethanol + Law + (1|Year) + (1|State))

m1f <- data.frame(Variable = rownames(summary(m1)$coef),
                  Coefficient = summary(m1)$coef[, 1],
                  SE = summary(m1)$coef[, 2],
                  modelName = "Homicidio")

m1f.n <- data.frame(Variable = rownames(summary(m1.n)$coef),
                    Coefficient = summary(m1.n)$coef[, 1],
                    SE = summary(m1.n)$coef[, 2],
                    modelName = "Homicidio")
  
m2 <- lmer(data = proj, 
           log(Legacy.rape..1) ~  povert_rate + Employ +unempl_rate + Gallons.of.ethanol + Dummy + (1|Year) + (1|State))
m2.n <- lmer(data = proj, 
           log(Legacy.rape..1) ~  povert_rate + Employ +unempl_rate + Gallons.of.ethanol + Law + (1|Year) + (1|State))


m2f <- data.frame(Variable = rownames(summary(m2)$coef),
                  Coefficient = summary(m2)$coef[, 1],
                  SE = summary(m2)$coef[, 2],
                  modelName = "Violación")

m2f.n <- data.frame(Variable = rownames(summary(m2.n)$coef),
                  Coefficient = summary(m2.n)$coef[, 1],
                  SE = summary(m2.n)$coef[, 2],
                  modelName = "Violación")

m3 <- lmer(data = proj, 
           log(Robbery) ~ povert_rate + Employ +unempl_rate + Gallons.of.ethanol + Dummy + (1|Year) + (1|State))
m3.n <- lmer(data = proj, 
           log(Robbery) ~ povert_rate + Employ +unempl_rate + Gallons.of.ethanol + Law + (1|Year) + (1|State))

m3f <- data.frame(Variable = rownames(summary(m3)$coef),
                  Coefficient = summary(m3)$coef[, 1],
                  SE = summary(m3)$coef[, 2],
                  modelName = "Robo")

m3f.n <- data.frame(Variable = rownames(summary(m3.n)$coef),
                  Coefficient = summary(m3.n)$coef[, 1],
                  SE = summary(m3.n)$coef[, 2],
                  modelName = "Robo")

m4 <- lmer(data = proj, 
           log(Aggravated.assault) ~ povert_rate + Employ +unempl_rate + Gallons.of.ethanol + Dummy + (1|Year) + (1|State))
m4.n <- lmer(data = proj, 
           log(Aggravated.assault) ~ povert_rate + Employ +unempl_rate + Gallons.of.ethanol + Law +  (1|Year) + (1|State))

m4f <- data.frame(Variable = rownames(summary(m4)$coef),
                  Coefficient = summary(m4)$coef[, 1],
                  SE = summary(m4)$coef[, 2],
                  modelName = "Asalto Agravado")

m4f.n <- data.frame(Variable = rownames(summary(m4.n)$coef),
                  Coefficient = summary(m4.n)$coef[, 1],
                  SE = summary(m4.n)$coef[, 2],
                  modelName = "Asalto Agravado")

sapply(lme4::ranef(m4.n),nrow)["State"]

Allmodels <- rbind(m1f, m2f, m3f, m4f)

Allmodels.n <- rbind(m1f.n, m2f.n, m3f.n, m4f.n)

interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2) # 95% multiplier
Allmodels$Variable <- revalue(Allmodels$Variable,
                              c("(Intercept)"="Intercept", 
                                "povert_rate"="Tasa de Pobreza",
                                "Employ"="Tasa de Empleo",
                                "unempl_rate"="Tasa de Desempleo",
                                "Gallons.of.ethanol"="Consumo Cerveza",
                                "Dummy"="Post-Ley"))

Allmodels.n$Variable <- revalue(Allmodels.n$Variable,
                              c("(Intercept)"="Intercept", 
                                "povert_rate"="Tasa de Pobreza",
                                "Employ"="Tasa de Empleo",
                                "unempl_rate"="Tasa de Desempleo",
                                "Gallons.of.ethanol"="Consumo Cerveza",
                                "Law"="Post-Ley"))

ggplot(Allmodels[Allmodels$Variable == "Tasa de Pobreza" | Allmodels$Variable == "Tasa de Empleo" | Allmodels$Variable == "Tasa de Desempleo" | Allmodels$Variable == "Consumo Cerveza"  | Allmodels$Variable == "Post-Ley", ], 
       aes(colour = modelName, shape=modelName)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                     ymax = Coefficient + SE*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*interval2,
                      ymax = Coefficient + SE*interval2),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  fill = "WHITE") + 
  coord_flip() + theme_bw() + 
  #  ggtitle("Coefficient Plot of ") +
  scale_colour_discrete(name="Tipo de Crimen") + 
  #scale_colour_grey(name="Tipo de Crimen")    para gris
  scale_shape_manual(name="Tipo de Crimen", values=c(0, 1, 2, 5, 6, 15, 16, 17, 18, 20)) +
  theme(title = element_text(face="italic", size=10, hjust=0)) +
  theme(axis.title.x = element_text(face="plain", size=12, hjust=.5)) +
  theme(axis.title.y = element_text(face="plain", size=12, hjust=.5)) +
  theme(legend.title = element_text(face="plain", size=12, hjust=.5)) +
  labs(caption = "\n\n All models include controls for democracy, Western Europe, 
       post-9/11 observations, and real GDP per capita, 
       \n though these are omitted here to save space.")


ggplot(Allmodels.n[Allmodels.n$Variable == "Tasa de Pobreza" | Allmodels.n$Variable == "Tasa de Empleo" | Allmodels.n$Variable == "Tasa de Desempleo" | Allmodels$Variable == "Consumo Cerveza" | Allmodels.n$Variable == "Post-Ley", ], 
       aes(colour = modelName, shape=modelName)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                     ymax = Coefficient + SE*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*interval2,
                      ymax = Coefficient + SE*interval2),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  fill = "WHITE") + 
  coord_flip() + theme_bw() + 
  #  ggtitle("Coefficient Plot of ") +
  scale_colour_discrete(name="Tipo de Crimen") + 
  scale_shape_manual(name="Tipo de Crimen", values=c(0, 1, 2, 5, 6, 15, 16, 17, 18, 20)) +
  theme(title = element_text(face="italic", size=10, hjust=0)) +
  theme(axis.title.x = element_text(face="plain", size=12, hjust=.5)) +
  theme(axis.title.y = element_text(face="plain", size=12, hjust=.5)) +
  theme(legend.title = element_text(face="plain", size=12, hjust=.5)) +
  labs(caption = "\n\n All models include controls for democracy, Western Europe, post-9/11 observations, and real GDP per capita, \n though these are omitted here to save space.")

#######

library(rddtools)
names(crime_t)
par(mfrow=c(2,2))

data <- rdd_data(y = crime_t$Murder.and.nonnegligent.Manslaughter/100000, x = crime_t$Year, cutpoint = 1998)
data <- rdd_data(y = crime_t$Murder.and.nonnegligent.Manslaughter, x = crime_t$Year, cutpoint = 1998)


data2 <- rdd_bw_ik(data)
rd2.lm <- rdd_reg_np(rdd_object = data, bw = data2)
#print(rd2.lm)
plot(rd2.lm, xlab = "", 
     ylab = "Tasa x 100k habit.", 
     cex = .2, 
     main = "Impacto de la legalización \n Asesinato",
     col = "blue")
abline(v = 1998)


data <- rdd_data(y = crime_t$Legacy.rape..1/100000, x = crime_t$Year, cutpoint = 1998)
data2 <- rdd_bw_ik(data)
rd2.lm <- rdd_reg_np(rdd_object = data, bw = data2)
#print(rd2.lm)
plot(rd2.lm, xlab = "", 
     ylab = "", 
     cex = .2, 
     main = "Impacto de la legalización \n Violación",
     col = "blue")
abline(v = 1998)


data <- rdd_data(y = crime_t$Robbery/100000, x = crime_t$Year, cutpoint = 1998)
data2 <- rdd_bw_ik(data)
rd2.lm <- rdd_reg_np(rdd_object = data, bw = data2)
#print(rd2.lm)
plot(rd2.lm, xlab = "Años", 
     ylab = "Tasa x 100k habitantes", 
     cex = .2, 
     main = "Impacto de la legalización \n Robo",
     col = "blue")
abline(v = 1998)


data <- rdd_data(y = crime_t$Aggravated.assault/100000, x = crime_t$Year, cutpoint = 1998)
data2 <- rdd_bw_ik(data)
rd2.lm <- rdd_reg_np(rdd_object = data, bw = data2)
#print(rd2.lm)
plot(rd2.lm, xlab = "Años", 
     ylab = "", 
     cex = .2, 
     main = "Impacto de la legalización \n Asalto Agravado",
     col = "blue")
abline(v = 1998)
# Bootstrapping
library(boot)

# Function to run RDDreg_lm for bootstrapping
rd <- function(data, i) {
  d <- data[i, ]
  r <- rdd_data(y = d$Murder.and.nonnegligent.Manslaughter, x = d$Year, cutpoint = 1998)
  d2 <- rdd_bw_ik(r)
  fit <- rdd_reg_np(rdd_object = r, bw = d2)
  return(fit$coefficients)
}

boot(crime_t, statistic = rd, 1000)
####
library(MASS)
data$treat <- ifelse(data$x <= 1998, 1, 0)

data$treat[data$treat == 1][sample(10)] <- 0
data$treat[data$treat == 0][sample(10)] <- 1

# Treatment effect
data$y[data$treat == 1] <- data$y[data$treat == 1] + 0.5
# Add grouping factor
data$group <- gl(2, 35/2)

library(RColorBrewer)
pal <- brewer.pal(5, "RdBu")

# Fit model
m <- lm(y ~ x + treat, data = data)

# predicted achievement for treatment group
pred_treat <- predict(m, 
                      newdata = data.frame(x = seq(-3, 0, 0.1), 
                                           treat = 1))
# predicted achievement for control group
pred_no_treat <- predict(m, 
                         newdata = data.frame(x = seq(0, 4, 0.1), 
                                              treat = 0))



##########
# Add legend
#legend("bottomright", 
#       legend = c("Treatment", "Control"),
#       lty = 1,
#       lwd = 2,
#       col = c(pal[4], pal[2]),
#       box.lwd = 0)


ggplot(data, aes(x = x, y = y, color = factor(treat, labels = c('Control', 'Treatment')))) + 
  geom_point(shape = 21) + 
  scale_color_brewer(NULL, type = 'qual', palette = 6) + 
  geom_vline(aes(xintercept = 1998), color = 'grey', size = 1, linetype = 'dashed') + 
  geom_segment(data = data.frame(t(predict(m, data.frame(x = c(1980, 1998), treat = 1)))), 
               aes(x = 1980, xend = 1998, y = X1, yend = X2), color = pal[4], size = 1) + 
  geom_segment(data = data.frame(t(predict(m, data.frame(x = c(1998, 2014), treat = 0)))), 
               aes(x = 1998, xend = 2014, y = X1, yend = X2), color = pal[2], size = 1)





rdd_mod <- rdd_reg_lm(rdd_object = data,
                      slope = "same")
summary(rdd_mod)


plot(rdd_mod,
     cex = 0.35,
     xlab="",
     ylab= "",
     main = "",
     col = "blue",
     sub = "SDSD SDSD Q",
     ann = FALSE)
#text(x=1990, y=100, "Add some text",col="gold4", font=2, cex=0.8)



##################
library(plm)
random <- plm(log(Murder.and.nonnegligent.Manslaughter) ~  povert_rate + Employ + unempl_rate + Gallons.of.ethanol + Dummy + factor(Year)
              , data=proj, index=c("State", "Year"), model="random")

fixed1 <- plm(log(Murder.and.nonnegligent.Manslaughter) ~  povert_rate + Employ + unempl_rate + Gallons.of.ethanol + Dummy + factor(Year)
              , data=proj, index=c("State", "Year"), model="within")

fixed1f <- data.frame(Variable = rownames(summary(fixed1)$coef),
                  Coefficient = summary(fixed1)$coef[, 1],
                  SE = summary(fixed1)$coef[, 2],
                  modelName = "Homicidio")

fixed2 <- plm(log(Legacy.rape..1) ~  povert_rate + Employ +unempl_rate + Gallons.of.ethanol + Dummy + factor(Year), 
              data=proj, index=c("State", "Year"), model="within")

fixed2f <- data.frame(Variable = rownames(summary(fixed2)$coef),
                      Coefficient = summary(fixed2)$coef[, 1],
                      SE = summary(fixed2)$coef[, 2],
                      modelName = "Homicidio")

Allmodels <- rbind(fixed1f, fixed2f)

interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2) # 95% multiplier
Allmodels$Variable <- revalue(Allmodels$Variable,
                              c("povert_rate"="Tasa de Pobreza",
                                "Employ"="Tasa de Empleo",
                                "unempl_rate"="Tasa de Desempleo",
                                "Gallons.of.ethanol"="Consumo Cerveza",
                                "Dummy"="Post-Ley",
                                "factor(Year)1993"="1993",
                                "factor(Year)1995"="1995",
                                "factor(Year)1996"="1996",
                                "factor(Year)1997"="1997",
                                "factor(Year)1998"="1998",
                                "factor(Year)1999"="1999",
                                "factor(Year)2000"="2000",
                                "factor(Year)2001"="2001",
                                "factor(Year)2002"="2002",
                                "factor(Year)2003"="2003",
                                "factor(Year)2004"="2004",
                                "factor(Year)2005"="2005",
                                "factor(Year)2006"="2006",
                                "factor(Year)2007"="2007",
                                "factor(Year)2008"="2008",
                                "factor(Year)2009"="2009",
                                "factor(Year)2010"="2010",
                                "factor(Year)2011"="2011",
                                "factor(Year)2012"="2012",
                                "factor(Year)2013"="2013",
                                "factor(Year)2014"="20014"))

#test de Hausman.... como el valor p es < a 0.05 -> Efectos fijos
phtest(fixed, random)




############# estados con mas población   CALIFORNIA
par(mfrow=c(1,3))
proj <- read.csv("C:/Users/Usuario/Documents/Doc español/Marihuana_law/data/Data_final.csv", 
                 stringsAsFactors = T, 
                 sep = ",")


proj <- filter(proj, State == "California" )
proj <- filter(proj, Year >= 1980 & Year <= 2014)
data <- rdd_data(y = proj$Violent.crime.total/100000, x = proj$Year, cutpoint = 1996)


data2 <- rdd_bw_ik(data)
rd2.lm <- rdd_reg_np(rdd_object = data, bw = data2)
#print(rd2.lm)
plot(rd2.lm, xlab = "", 
     ylab = "Tasa x 100k habit.", 
     cex = .2, 
     main = "Impacto de la LM sobre el \n Crimen en California",
     col = "blue")
abline(v = 1995)

############# estados con mas población   Texas
proj <- read.csv("C:/Users/Usuario/Documents/Doc español/Marihuana_law/data/Data_final.csv", 
                 stringsAsFactors = T, 
                 sep = ",")
proj <- filter(proj, State == "Texas" )
proj <- filter(proj, Year >= 1980 & Year <= 2014)
data <- rdd_data(y = proj$Violent.crime.total/100000, x = proj$Year, cutpoint = 1996)


data2 <- rdd_bw_ik(data)
rd2.lm <- rdd_reg_np(rdd_object = data, bw = data2)
#print(rd2.lm)
plot(rd2.lm, xlab = "", 
     ylab = "Tasa x 100k habit.", 
     cex = .2, 
     main = "Impacto de la LM sobre el \n Crimen en Texas",
     col = "blue")
abline(v = 1995)

################  New Yearsy
proj <- read.csv("C:/Users/Usuario/Documents/Doc español/Marihuana_law/data/Data_final.csv", 
                 stringsAsFactors = T, 
                 sep = ",")

proj <- filter(proj, State == "New Jersey" )
proj <- filter(proj, Year >= 1980 & Year <= 2014)
data <- rdd_data(y = proj$Violent.crime.total/100000, x = proj$Year, cutpoint = 1996)


data2 <- rdd_bw_ik(data)
rd2.lm <- rdd_reg_np(rdd_object = data, bw = data2)
#print(rd2.lm)
plot(rd2.lm, xlab = "", 
     ylab = "Tasa x 100k habit.", 
     cex = .2, 
     main = "Impacto de la LM sobre el \n Crimen en New Jersey",
     col = "blue")
abline(v = 1996)

