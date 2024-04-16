#Opening packages and clearing environment 
rm(list=ls())
library(BiodiversityR)
library(vegan)
library(permute)
library(lattice)
library(tcltk)
library(tidyverse)
library(insight)
library(readxl)
library(tidyverse)
library(plyr)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(extrafont)
library(ggpubr)
library(ade4)
library(lme4)
library(MuMIn)
library(factoextra)
library(effectsize)
library(MASS)
library(klaR)
library(psych)
library(devtools)
library(gridExtra)
library(psycho)
library(report)
library(multcomp)
library(car)

#setting working directory and opening dataset
setwd ("C:\\Users\\sabin\\Documents\\Uni\\Year 4\\Dissertation\\Data")
weight <- read_excel("weight.xlsx")

#converting characters to factors 
weight$tray <- as.factor(weight$tray)
weight$seed <- as.factor(weight$seed)
weight$fertiliser <- as.factor(weight$fertiliser)
weight$treatment <- as.factor(weight$treatment)
weight$type <- as.factor(weight$type)

#Creating a dataset without tray 5
weight_2 <- filter(weight, tray %in% c(1,2,3,4))


# Two way/one way ANOVA - biomass and AA ------------------------------------------

#Creating datasets without WT-C for two way ANOVA
no_control <- filter(weight_2, fertiliser %in% c("on", "in"))
no_control_all <- filter(weight, fertiliser %in% c("on", "in"))

#root shoot 
anova_rootshoot <- aov(root_shoot~seed*fertiliser, data = no_control)
report(anova_rootshoot)
summary(anova_rootshoot)

anova_rootshootall <- aov(root_shoot~seed*fertiliser, data = no_control_all)
report(anova_rootshootall)


#The ANOVA suggests that:

#The main effect of seed is statistically not significant and medium
#(F(1, 22) = 2.38, p = 0.138; Eta2 (partial) = 0.10, 95% CI [0.00,1.00])

#The main effect of fertiliser is statistically significant and large
#(F(1, 22) = 5.48, p = 0.029; Eta2 (partial) = 0.21, 95% CI [0.01, 1.00])

#is an interaction appropriate? dont think so 

#Effect sizes were labelled following Field's (2013) recommendations. 

#Tukey multiple pairwise-comparisons

TukeyHSD(anova_rootshoot, which = "fertiliser")

#diff       lwr       upr     p adj
#on-in 0.01720315 0.0019937 0.0324126 0.0284128


anova_rootshoot2 <- aov(root_shoot~treatment, data = weight_2) 
report(anova_rootshoot2)

anova_rootshoot3 <- aov(root_shoot~treatment, data = control_wt_2) 
report(anova_rootshoot3)

anova_rootshoot4 <- aov(root_shoot~treatment, data = weight) 
report(anova_rootshoot4)


plot(anova_rootshoot2, 1)
#Levene's test for homogeneity of variances 
leveneTest(root_shoot~treatment, data = weight_2)
#seems homog p = 0.42 and individually

#checking normality
plot(anova_rootshoot2, 2)
#assume normality
rootshoot_resid <- resid(anova_rootshoot2)
shapiro.test(rootshoot_resid)
#W = 0.944, p = 0.10


#root weight 
anova_rootweight <- aov(root_weight~seed*fertiliser, data = no_control)
report(anova_rootweight)

anova_rootweight_all <- aov(root_weight~seed*fertiliser, data = no_control_all)
report(anova_rootweight_all)

#tukey to find out which higher?
TukeyHSD(anova_rootweight, which = "seed")
#wt higher 

#should perhaps be an interaction ? since we suspect combinations
#of variables to differ?

#The ANOVA (formula: root_weight ~ seed * fertiliser) suggests that:

#The main effect of seed is statistically significant and large 
#(F(1,21) = 133.19, p < .001; Eta2 (partial) = 0.86, 95% CI [0.76, 1.00])

#The main effect of fertiliser is statistically not significant and
#small (F(1, 21) = 1.25, p = 0.277; Eta2 (partial) = 0.06, 95% CI[0.00, 1.00])

#The interaction between seed and fertiliser is statistically significant 
#and large (F(1, 21) = 4.35, p = 0.049; Eta2 (partial) =0.17, 95% CI [2.94e-04, 1.00])

#one-way including wt_c
anova_rootweight2 <- aov(root_weight~treatment, data = weight_2) 
report(anova_rootweight2)

anova_rootweight3 <- aov(root_weight~treatment, data = control_wt_2) 
report(anova_rootweight3)
# - The main effect of treatment is statistically significant and large
#(F(4, 27) = 46.02, p < .001; Eta2 = 0.87, 95% CI [0.78, 1.00])

anova_rootweight4 <- aov(root_weight~treatment, data = weight) 
report(anova_rootweight4)

TukeyHSD(anova_rootweight2, which = "treatment")
TukeyHSD(anova_rootweight3, which = "treatment")
TukeyHSD(anova_rootweight4, which = "treatment")


#all p < 0.001 except:
#ko-in/ko-on
#wt-in/wt-c
#wt-on/wt-c
#wt-on/wt-in

leveneTest(root_weight~seed*fertiliser, data = weight_2)
#p = 0.248

rootweight_resid <- resid(anova_rootweight)
shapiro.test(rootweight_resid)
#W = 0.9731, p = 0.72

#leaf weight
anova_leafweight <- aov(leaf_weight~seed*fertiliser, data = no_control)
report(anova_leafweight)

anova_leafweight_all <- aov(leaf_weight~seed*fertiliser, data = no_control_all)
report(anova_leafweight_all)
#The ANOVA (formula: leaf_weight ~ seed * fertiliser) suggests that:

#The main effect of seed is statistically significant and large 
#(F(1,21) = 123.63, p < .001; Eta2 (partial) = 0.85, 95% CI [0.74, 1.00])

#The main effect of fertiliser is statistically not significant and
#medium (F(1, 21) = 3.17, p = 0.089; Eta2 (partial) = 0.13, 95% CI[0.00, 1.00])

#The interaction between seed and fertiliser is statistically not
#significant and small (F(1, 21) = 0.97, p = 0.335; Eta2 (partial) =0.04, 95% CI [0.00, 1.00])

#Effect sizes were labelled following Field's (2013) recommendations.

TukeyHSD(anova_leafweight, which = "seed")
#wt higher p < 0.001
TukeyHSD(anova_leafweight, which = "fertiliser")
#in higher (not signif) p = 0.10

#one-way including wt_c
anova_leafweight2 <- aov(leaf_weight~treatment, data = weight_2) 
report(anova_leafweight2)

anova_leafweight3 <- aov(leaf_weight~treatment, data = weight) 
report(anova_leafweight3)

#The ANOVA (formula: leaf_weight ~ treatment) suggests that:

#The main effect of treatment is statistically significant and large
#(F(4, 27) = 36.26, p < .001; Eta2 = 0.84, 95% CI [0.74, 1.00])
TukeyHSD(anova_leafweight2, which = "treatment")
TukeyHSD(anova_leafweight3, which = "treatment")


#all p < 0.001 except:
#ko-in/ko-on 
#wt-on/wt-c
#wt-on/wt-in
#but only focus on wt-c things here 

leveneTest(leaf_weight~seed*fertiliser, data = no_control)
#p = 0.09 **

leafweight_resid <- resid(anova_leafweight)
shapiro.test(leafweight_resid)
#W = 0.9529, p = 0.2915

#one way
leveneTest(leaf_weight~treatment, data = weight_2)
#p = 0.11

leafweight2_resid <- resid(anova_leafweight2)
shapiro.test(leafweight2_resid)
#W = 0.9594, p = 0.2645


#stem weight
anova_stemweight <- aov(stem_weight~seed*fertiliser, data = no_control)
report(anova_stemweight)

anova_stemweight_all <- aov(stem_weight~seed*fertiliser, data = no_control_all)
report(anova_stemweight_all)
#The ANOVA (formula: stem_weight ~ seed * fertiliser) suggests that:

#The main effect of seed is statistically significant and large 
#(F(1,21) = 23.46, p < .001; Eta2 (partial) = 0.53, 95% CI [0.26, 1.00])

#The main effect of fertiliser is statistically not significant and
#very small (F(1, 21) = 0.12, p = 0.735; Eta2 (partial) = 5.59e-03, 95% CI [0.00, 1.00])

#The interaction between seed and fertiliser is statistically not
#significant and medium (F(1, 21) = 2.94, p = 0.101; Eta2 (partial) =
#0.12, 95% CI [0.00, 1.00])

TukeyHSD(anova_stemweight, which = "seed")
#wt higher p adj = 8.67e-05
TukeyHSD(anova_stemweight, which = "fertiliser")
#on higher (not signif) p = 0.74

#one-way including wt_c
anova_stemweight2 <- aov(stem_weight~treatment, data = weight_2) 
report(anova_stemweight2)

anova_stemweight3 <- aov(stem_weight~treatment, data = control_wt_2) 
report(anova_stemweight3)

anova_stemweight4 <- aov(stem_weight~treatment, data = weight) 
report(anova_stemweight4)
anova_stemweight5 <- aov(stem_weight~treatment, data = control_wt) 
report(anova_stemweight5)
#The main effect of treatment is statistically significant and large
#(F(4, 27) = 7.98, p < .001; Eta2 = 0.54, 95% CI [0.27, 1.00])

TukeyHSD(anova_stemweight2, which = "treatment")
TukeyHSD(anova_stemweight3, which = "treatment")
TukeyHSD(anova_stemweight4, which = "treatment")

#no significant difference between wt-c and any other treatment in
#stem weight 


leveneTest(stem_weight~seed*fertiliser, data = no_control)
#p = 0.766

stemweight_resid <- resid(anova_stemweight)
shapiro.test(stemweight_resid)
#W = 0.9527, p = 0.2888

#one way
leveneTest(stem_weight~treatment, data = weight_2)
#p = 0.55

stemweight2_resid <- resid(anova_stemweight2)
shapiro.test(stemweight2_resid)
#W = 0.9579, p = 0.2402

#above ground biomass
anova_shootweight <- aov(total_shoot_weight~seed*fertiliser, data = no_control)
report(anova_shootweight)

anova_shootweight_all <- aov(total_shoot_weight~seed*fertiliser, data = no_control_all)
report(anova_shootweight_all)
#The ANOVA (formula: total_shoot_weight ~ seed * fertiliser) suggests that:

#The main effect of seed is statistically significant and large 
#(F(1,21) = 128.50, p < .001; Eta2 (partial) = 0.86, 95% CI [0.75, 1.00])

#The main effect of fertiliser is statistically not significant and
#small (F(1, 21) = 0.34, p = 0.567; Eta2 (partial) = 0.02, 95% CI [0.00, 1.00])

#The interaction between seed and fertiliser is statistically not
#significant and medium (F(1, 21) = 1.85, p = 0.188; Eta2 (partial) =
#0.08, 95% CI [0.00, 1.00])

TukeyHSD(anova_shootweight, which = "seed")
#wt higher p adj = < 0.001
TukeyHSD(anova_shootweight, which = "fertiliser")
#in higher (not signif) p = 0.57

#one-way including wt_c
anova_shootweight2 <- aov(total_shoot_weight~treatment, data = weight_2) 
report(anova_shootweight2)

anova_shootweight3 <- aov(total_shoot_weight~treatment, data = weight) 
report(anova_shootweight3)
#The main effect of treatment is statistically significant and large
#(F(4, 27) = 36.97, p < .001; Eta2 = 0.85, 95% CI [0.74, 1.00])

TukeyHSD(anova_shootweight2, which = "treatment")
TukeyHSD(anova_shootweight3, which = "treatment")

#wt-on higher than wt-c p = 0.019
#wt-c higher than ko-in or ko-on (p = < 0.001)
#wt-in and w-c not different 

#try wt only 
anova_shootweight4 <- aov(total_shoot_weight~treatment, data = control_wt_2)
report(anova_shootweight4)
summary(anova_shootweight4)

anova_shootweight5 <- aov(total_shoot_weight~treatment, data = control_wt)
report(anova_shootweight5)

TukeyHSD(anova_shootweight4, which = "treatment")
TukeyHSD(anova_shootweight5, which = "treatment")

mean_agb_wtc <- mean(wt_c2$total_shoot_weight)
mean_agb_wtc
mean_agb_wtin <- mean(wt_in2$total_shoot_weight)
mean_agb_wtin
mean_agb_wton <- mean(wt_on2$total_shoot_weight)
mean_agb_wton

mean_tb_wtc <- mean(wt_c2$total_biomass)
mean_tb_wtc
mean_tb_wtin <- mean(wt_in2$total_biomass)
mean_tb_wtin
mean_tb_wton <- mean(wt_on2$total_biomass)
mean_tb_wton

summary(wt_c2$total_biomass)
344.7 - 266.3
#range = 78.4

summary(wt_c2$total_biomass)


((mean_agb_wtin - mean_agb_wtc)/mean_agb_wtc)*100
#wt-in 21.9% higher AGB than wt-c 

((mean_agb_wton - mean_agb_wtc)/mean_agb_wtc)*100
#wt-on 27.2% higher AGB than wt-c

((mean_tb_wtin - mean_tb_wtc)/mean_tb_wtc)*100
#wt-in 16.4% higher tb than wt-c

((mean_tb_wton - mean_tb_wtc)/mean_tb_wtc)*100
#wt-on 23.4% higher tb than wt-c 

#wt-in still not higher than wt-c

leveneTest(total_shoot_weight~seed*fertiliser, data = no_control)
#p = 0.381

shootweight_resid <- resid(anova_shootweight)
shapiro.test(shootweight_resid)
#W = 0.9761, p = 0.7979

#one way
leveneTest(total_shoot_weight~treatment, data = weight_2)
#p = 0.43

shootweight2_resid <- resid(anova_shootweight2)
shapiro.test(shootweight2_resid)
#W = 0.9816, p = 0.8453

#total biomass
anova_totalweight <- aov(total_biomass~seed*fertiliser, data = no_control)
report(anova_totalweight)

anova_totalweight2 <- aov(total_biomass~seed*fertiliser, data = no_control_all)
report(anova_totalweight2)

#The ANOVA (formula: total_biomass ~ seed * fertiliser) suggests that:

#The main effect of seed is statistically significant and large 
#(F(1,21) = 137.20, p < .001; Eta2 (partial) = 0.87, 95% CI [0.76, 1.00])

#The main effect of fertiliser is statistically not significant and
#very small (F(1, 21) = 0.15, p = 0.703; Eta2 (partial) = 7.05e-03, 95% CI [0.00, 1.00])

#The interaction between seed and fertiliser is statistically not
#significant and medium (F(1, 21) = 2.23, p = 0.150; Eta2 (partial) =
#0.10, 95% CI [0.00, 1.00])

TukeyHSD(anova_totalweight, which = "seed")
#wt higher p adj = 0
TukeyHSD(anova_totalweight, which = "fertiliser")
#in higher (not signif) p = 0.71

#one-way including wt_c
anova_totalweight3 <- aov(total_biomass~treatment, data = weight_2) 
report(anova_totalweight3)

anova_totalweight4 <- aov(total_biomass~treatment, data = weight) 
report(anova_totalweight4)

anova_totalweight5 <- aov(total_biomass~treatment, data = control_wt_2) 
report(anova_totalweight5)

#The main effect of treatment is statistically significant and large
#(F(4, 27) = 42.27, p < .001; Eta2 = 0.86, 95% CI [0.77, 1.00])

TukeyHSD(anova_totalweight3, which = "treatment")
TukeyHSD(anova_totalweight4, which = "treatment")
TukeyHSD(anova_totalweight5, which = "treatment")

#wt-on signif higher total biomass than wt-c (p = 0.031)
#wt-c higher than ko-in or ko-on (p < 0.001)
#i am concerned that this is weirdly weighted 
#anova with just the wt?

anova_totalweight3 <- aov(total_biomass~treatment, data = control_wt_2) 
report(anova_totalweight3)

TukeyHSD(anova_totalweight3, which = "treatment")
#wt-in still not significantly larger than wt-c...

leveneTest(total_biomass~seed*fertiliser, data = no_control)
#p = 0.41

totalweight_resid <- resid(anova_totalweight)
shapiro.test(totalweight_resid)
#W = 0.970, p = 0.646

#one way
leveneTest(total_biomass~treatment, data = weight_2)
#p = 0.37

totalweight3_resid <- resid(anova_totalweight3)
shapiro.test(totalweight3_resid)
#W = 0.970, p = 0.781



#total amino acid content

anova_totalaa <- aov(all_aa~seed*fertiliser, data = no_control)
report(anova_totalaa)

anova_totalaa2 <- aov(all_aa~seed*fertiliser, data = no_control_all)
report(anova_totalaa2)

#The ANOVA (formula: all_aa ~ seed * fertiliser) suggests that:

#The main effect of seed is statistically not significant and medium 
#(F(1,19) = 2.64, p < .121; Eta2 (partial) = 0.12, 95% CI [0.00, 1.00])

#The main effect of fertiliser is statistically not significant and
#very small (F(1, 19) = 0.05, p = 0.824; Eta2 (partial) = 2.66e-03, 95% CI [0.00, 1.00])

#The interaction between seed and fertiliser is statistically
#significant and large (F(1, 19) = 5.72, p = 0.027; Eta2 (partial) =
#0.23, 95% CI [0.02, 1.00])

TukeyHSD(anova_totalaa, which = "seed")
TukeyHSD(anova_totalaa2, which = "seed")
#wt higher p adj = 0.12
TukeyHSD(anova_totalaa, which = "fertiliser")
#in higher (not signif) p = 0.83

#one-way including wt_c
anova_totalaa3 <- aov(all_aa~treatment, data = weight_2) 
report(anova_totalaa3)

anova_totalaa4 <- aov(all_aa~treatment, data = weight) 
report(anova_totalaa4)

anova_totalaa5 <- aov(all_aa~treatment, data = no_control) 
report(anova_totalaa5)

anova_totalaa6 <- aov(all_aa~treatment, data = control_wt) 
report(anova_totalaa6)
#The main effect of treatment is statistically significant and large
#(F(4, 27) = 42.27, p < .001; Eta2 = 0.86, 95% CI [0.77, 1.00])

TukeyHSD(anova_totalaa3, which = "treatment")
TukeyHSD(anova_totalaa4, which = "treatment")
TukeyHSD(anova_totalaa5, which = "treatment")
TukeyHSD(anova_totalaa6, which = "treatment")

#wt-on signif higher total biomass than wt-c (p = 0.031)
#wt-c higher than ko-in or ko-on (p < 0.001)
#i am concerned that this is weirdly weighted 
#anova with just the wt?

anova_totalweight3 <- aov(total_biomass~treatment, data = control_wt_2) 
report(anova_totalweight3)

TukeyHSD(anova_totalweight3, which = "treatment")
#wt-in still not significantly larger than wt-c...

leveneTest(all_aa~seed*fertiliser, data = no_control)
#p = 0.59

totalaa_resid <- resid(anova_totalaa)
shapiro.test(totalaa_resid)
#W = 0.956, p = 0.394

#one way
leveneTest(all_aa~treatment, data = weight_2)
#p = 0.69

totalaa3_resid <- resid(anova_totalaa3)
shapiro.test(totalaa3_resid)
#W = 0.957, p = 0.283


#Root amino acid content

anova_rootaa <- aov(total_r~seed*fertiliser, data = no_control)
report(anova_rootaa)

anova_rootaa2 <- aov(total_r~seed*fertiliser, data = no_control_all)
report(anova_rootaa2)

#The ANOVA (formula: total_r ~ seed * fertiliser) suggests that:

#The main effect of seed is statistically not significant and very small 
#(F(1,19) = 0.12, p < 0.738; Eta2 (partial) = 6.02e-03, 95% CI [0.00, 1.00])

#The main effect of fertiliser is statistically not significant and
#medium (F(1, 19) = 1.61, p = 0.219; Eta2 (partial) = 0.08, 95% CI [0.00, 1.00])

#The interaction between seed and fertiliser is statistically
#significant and large (F(1, 19) = 4.90, p = 0.039; Eta2 (partial) =
#0.20, 95% CI [0.02, 1.00])

TukeyHSD(anova_rootaa, which = "seed")
TukeyHSD(anova_totalaa2, which = "seed")
#wt higher p adj = 0.74
TukeyHSD(anova_rootaa, which = "fertiliser")
#in higher (not signif) p = 0.23

#one-way including wt_c
anova_rootaa3 <- aov(total_r~treatment, data = weight_2) 
report(anova_rootaa3)

anova_rootaa4 <- aov(total_r~treatment, data = weight) 
report(anova_rootaa4)

anova_rootaa5 <- aov(total_r~treatment, data = control_wt_2) 
report(anova_rootaa5)

anova_rootaa6 <- aov(total_r~treatment, data = control_wt) 
report(anova_rootaa6)
#The main effect of treatment is statistically significant and large
#(F(4, 27) = 42.27, p < .001; Eta2 = 0.86, 95% CI [0.77, 1.00])

TukeyHSD(anova_rootaa3, which = "treatment")
TukeyHSD(anova_rootaa4, which = "treatment")
TukeyHSD(anova_rootaa5, which = "treatment")
TukeyHSD(anova_rootaa6, which = "treatment")

#wt-on signif higher total biomass than wt-c (p = 0.031)
#wt-c higher than ko-in or ko-on (p < 0.001)
#i am concerned that this is weirdly weighted 
#anova with just the wt?

anova_totalweight3 <- aov(total_biomass~treatment, data = control_wt_2) 
report(anova_totalweight3)

TukeyHSD(anova_totalweight3, which = "treatment")
#wt-in still not significantly larger than wt-c...

leveneTest(total_r~seed*fertiliser, data = no_control)
#p = 0.65

rootaa_resid <- resid(anova_rootaa)
shapiro.test(rootaa_resid)
#W = 0.937, p = 0.16

#one way
leveneTest(total_r~treatment, data = weight_2)
#p = 0.50

rootaa3_resid <- resid(anova_rootaa3)
shapiro.test(rootaa3_resid)
#W = 0.935, p = 0.074

#leaf aa
anova_leafaa <- aov(total_l~seed*fertiliser, data = no_control)
report(anova_leafaa)

anova_leafaa2 <- aov(total_l~seed*fertiliser, data = no_control_all)
report(anova_leafaa2)

#The ANOVA (formula: total_r ~ seed * fertiliser) suggests that:

#The main effect of seed is statistically significant and large
#(F(1,20) = 12.25, p = 0.002; Eta2 (partial) = 0.38 95% CI [0.00, 1.00])

#The main effect of fertiliser is statistically not significant and
#medium (F(1, 20) = 1.32, p = 0.265; Eta2 (partial) = 0.06, 95% CI [0.00, 1.00])

#The interaction between seed and fertiliser is not statistically
#significant and medium (F(1, 20) = 2.43, p = 0.135; Eta2 (partial) =
#0.11, 95% CI [0.02, 1.00])

TukeyHSD(anova_leafaa, which = "seed")
TukeyHSD(anova_totalaa2, which = "seed")
#wt higher p adj = 0.74
TukeyHSD(anova_leafaa, which = "fertiliser")
#in higher (not signif) p = 0.23

#one-way including wt_c
anova_leafaa3 <- aov(total_l~treatment, data = weight_2) 
report(anova_leafaa3)

anova_leafaa4 <- aov(total_l~treatment, data = weight) 
report(anova_leafaa4)

anova_rootaa5 <- aov(total_r~treatment, data = control_wt_2) 
report(anova_rootaa5)

anova_rootaa6 <- aov(total_r~treatment, data = control_wt) 
report(anova_rootaa6)
#The main effect of treatment is statistically significant and large
#(F(4, 27) = 42.27, p < .001; Eta2 = 0.86, 95% CI [0.77, 1.00])

TukeyHSD(anova_leafaa3, which = "treatment")
TukeyHSD(anova_leafaa4, which = "treatment")
TukeyHSD(anova_rootaa5, which = "treatment")
TukeyHSD(anova_rootaa6, which = "treatment")

#wt-on signif higher total biomass than wt-c (p = 0.031)
#wt-c higher than ko-in or ko-on (p < 0.001)
#i am concerned that this is weirdly weighted 
#anova with just the wt?

anova_totalweight3 <- aov(total_biomass~treatment, data = control_wt_2) 
report(anova_totalweight3)

TukeyHSD(anova_totalweight3, which = "treatment")
#wt-in still not significantly larger than wt-c...

leveneTest(total_r~seed*fertiliser, data = no_control)
#p = 0.65

rootaa_resid <- resid(anova_rootaa)
shapiro.test(rootaa_resid)
#W = 0.937, p = 0.16

#one way
leveneTest(total_r~treatment, data = weight_2)
#p = 0.50

rootaa3_resid <- resid(anova_rootaa3)
shapiro.test(rootaa3_resid)



# NMDS - ordination and plotting ------------------------------------------

root_aa <- weight_2[,c(5, 19:36)]
root_aa <- na.omit(root_aa)
root_aa2 <- root_aa[-c(1)]
root_aa_groups <- root_aa$treatment
nrow(root_aa2)

root_aa3 <- root_aa2[,-c(8,17)]
grp_root <- rep(NA,29)

colnames(root_aa3) <- new_row_names


root.nmds <- metaMDS(root_aa3)
root.nmds$stress
#stress = 0.092
root.nmds
plot(root.nmds)

root.scores <- as.data.frame(scores(root.nmds, "sites"))
root.scores$site <- rownames(root.scores)
root.scores$root_aa_groups <- root_aa$treatment
head(root.scores)

root.scores$root_aa_groups <- as.factor(root.scores$root_aa_groups)

root.species <- as.data.frame(scores(root.nmds, "species"))
root.species$species <- rownames(root.species)

head(root.species)
new_row_names <- c("Asp", "Glu", "His", "Ser", "Thr", "Gly", "Arg", "Ala", "Tyr", "Gaba", "Met", "Val", "Phe", "Ile", "Leu", "Lys")
rownames(root.species) <- new_row_names

ggplot()+
  geom_polygon(data = hull.data.rootnew, aes (x = NMDS1, y = NMDS2, fill = root_aa_groups, group = root_aa_groups), alpha = 0.5)+
  geom_text(data = root.species, aes( x = NMDS1, y = NMDS2, label = species), alpha = 1, size = 6)+
  geom_point(data = root.scores, aes( x = NMDS1, y = NMDS2,colour = root_aa_groups),size = 4, show.legend = FALSE)+
  scale_color_manual(values = my_colours)+
  scale_fill_manual(values = my_colours, name = "Treatment",labels = c("KO-IN", "KO-ON", "WT-C","WT-IN", "WT-ON"))+
  coord_equal()+
  labs(x = "Root NMDS1", y = "Root NMDS2")+
  theme_classic()+
  theme(
    axis.title.x = element_text(size = 20, margin = margin(t = 20)),
    axis.title.y = element_text(size = 20, margin = margin(r = 20), family = "Arial"),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.text = element_text(size = 18, color = "black"),
    text = element_text(family = "Arial", color = "black"),
    legend.title = element_blank(),
    legend.spacing.x = unit(0.5, "cm"),
    legend.position = "top",
    plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))+
  guides(fill = guide_legend(byrow = TRUE))


ggsave("nmds_rootnew_p1.png",
       height = 21.24, width = 30.41,units='cm')

ggsave("nmds_rootnew_p2.png",
       height = 21.24, width = 30.41,units='cm')

ggsave("nmds_rootnew_p3.png",
       height = 21.24, width = 30.41,units='cm')
#save one version with and one without aa labels?


m_com = as.matrix(root_aa3)
m_com

ano = anosim(m_com, root_aa$treatment, distance = "bray", permutations = 9999)
summary(ano)

#ANOSIM statistic R = 0.5982
#significance = 1e-04

library(indicspecies)
group <- root_aa$treatment

inv = multipatt(root_aa3, group, func = "r.g", control = how(nperm=9999), alpha = 1)
summary(inv, alpha = 1)

simper.root <- simper(root_aa3, root_aa$treatment)
summary(simper.root)

grp.wt_on <- root.scores[root.scores$root_aa_groups == "wt_on",][chull(root.scores[root.scores$root_aa_groups == "wt_on", c("NMDS1", "NMDS2")]),]
grp.wt_in <- root.scores[root.scores$root_aa_groups == "wt_in",][chull(root.scores[root.scores$root_aa_groups == "wt_in", c("NMDS1", "NMDS2")]),]
grp.ko_on <- root.scores[root.scores$root_aa_groups == "ko_on",][chull(root.scores[root.scores$root_aa_groups == "ko_on", c("NMDS1", "NMDS2")]),]
grp.ko_in <- root.scores[root.scores$root_aa_groups == "ko_in",][chull(root.scores[root.scores$root_aa_groups == "ko_in", c("NMDS1", "NMDS2")]),]
grp.wt_c <- root.scores[root.scores$root_aa_groups == "wt_c",][chull(root.scores[root.scores$root_aa_groups == "wt_c", c("NMDS1", "NMDS2")]),]

hull.data.rootnew <- rbind(grp.wt_on,grp.wt_in,grp.ko_on,grp.ko_in,grp.wt_c)
hull.data.rootnew  



# New Leaf ----------------------------------------------------------------
leaf_aa <- weight_2[, c(5, 39:55)]
leaf_aa <- na.omit(leaf_aa)
leaf_aa_groups <- leaf_aa$treatment

leaf_aa2 <- leaf_aa[,-c(1)]
leaf_aa3 <- leaf_aa2[,-c(8,17)]


colnames(leaf_aa3) <- new_row_names


leaf.nmds <- metaMDS(leaf_aa3)
leaf.nmds$stress
#stress = 0.099
leaf.nmds
plot(leaf.nmds)

leaf.scores <- as.data.frame(scores(leaf.nmds, "sites"))
leaf.scores$site <- rownames(leaf.scores)
leaf.scores$leaf_aa_groups <- leaf_aa$treatment
head(leaf.scores)

leaf.scores$leaf_aa_groups <- as.factor(leaf.scores$leaf_aa_groups)

leaf.species <- as.data.frame(scores(leaf.nmds, "species"))
leaf.species$species <- rownames(leaf.species)

head(leaf.species)
new_row_names <- c("Asp", "Glu", "His", "Ser", "Thr", "Gly", "Arg", "Ala", "Tyr", "Gaba", "Met", "Val", "Phe", "Ile", "Leu", "Lys")
rownames(root.species) <- new_row_names

ggplot()+
  geom_polygon(data = hull.data.leafnew, aes (x = NMDS1, y = NMDS2, fill = leaf_aa_groups, group = leaf_aa_groups), alpha = 0.5)+
  geom_text(data = leaf.species, aes( x = NMDS1, y = NMDS2, label = species), alpha = 1, size = 6)+
  geom_point(data = leaf.scores, aes( x = NMDS1, y = NMDS2,colour = leaf_aa_groups),size = 4, show.legend = FALSE)+
  scale_color_manual(values = my_colours)+
  scale_fill_manual(values = my_colours, name = "Treatment",labels = c("KO-IN", "KO-ON", "WT-C","WT-IN", "WT-ON"))+
  coord_equal()+
  labs(x = "Leaf NMDS1", y = "Leaf NMDS2")+
  theme_classic()+
  theme(
    axis.title.x = element_text(size = 20, margin = margin(t = 20)),
    axis.title.y = element_text(size = 20, margin = margin(r = 20), family = "Arial"),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.text = element_text(size = 18, color = "black"),
    text = element_text(family = "Arial", color = "black"),
    legend.title = element_blank(),
    legend.spacing.x = unit(0.5, "cm"),
    legend.position = "top",
    plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))+
  guides(fill = guide_legend(byrow = TRUE))


ggsave("nmds_leafnew_p1.png",
       height = 21.24, width = 30.41,units='cm')

ggsave("nmds_leafnew_p3.png",
       height = 21.24, width = 30.41,units='cm')
#save one version with and one without aa labels?


m_com_leaf = as.matrix(leaf_aa3)
m_com_leaf

ano_leaf = anosim(m_com_leaf, leaf_aa$treatment, distance = "bray", permutations = 9999)
ano_leaf

#ANOSIM statistic R = 0.5239
#significance = 1e-04

library(indicspecies)
group <- leaf_aa$treatment

inv_leaf = multipatt(leaf_aa3, group, func = "r.g", control = how(nperm=9999))
summary(inv_leaf, alpha = 1)

simper.root <- simper(root_aa3, root_aa$treatment_numeric)
summary(simper.root)

#gaba and ala only associated to wt-on 
#p = 0.0253 for ala 
#how to interpret exactly 

anova_ala <- aov(ala_l~seed*fertiliser, data = no_control)
summary(anova_ala)
TukeyHSD(anova_ala, which = "fertiliser")

ggplot(data = no_control, aes(x = treatment, y = ala_l))+
  geom_bar(stat = "identity", position = "dodge")

grp.wt_on_leaf <- leaf.scores[leaf.scores$leaf_aa_groups == "wt_on",][chull(leaf.scores[leaf.scores$leaf_aa_groups == "wt_on", c("NMDS1", "NMDS2")]),]
grp.wt_in_leaf <- leaf.scores[leaf.scores$leaf_aa_groups == "wt_in",][chull(leaf.scores[leaf.scores$leaf_aa_groups == "wt_in", c("NMDS1", "NMDS2")]),]
grp.ko_on_leaf <- leaf.scores[leaf.scores$leaf_aa_groups == "ko_on",][chull(leaf.scores[leaf.scores$leaf_aa_groups == "ko_on", c("NMDS1", "NMDS2")]),]
grp.ko_in_leaf <- leaf.scores[leaf.scores$leaf_aa_groups == "ko_in",][chull(leaf.scores[leaf.scores$leaf_aa_groups == "ko_in", c("NMDS1", "NMDS2")]),]
grp.wt_c_leaf <- leaf.scores[leaf.scores$leaf_aa_groups == "wt_c",][chull(leaf.scores[leaf.scores$leaf_aa_groups == "wt_c", c("NMDS1", "NMDS2")]),]

hull.data.leafnew <- rbind(grp.wt_on_leaf,grp.wt_in_leaf,grp.ko_on_leaf,grp.ko_in_leaf,grp.wt_c_leaf)
hull.data.rootnew  



# Leaf - seed and fertiliser ----------------------------------------------
leaf_aa <- weight_2[, c(5, 39:55)]
leaf_aa <- na.omit(leaf_aa)
leaf_aa_fertiliser <- leaf_aa5$fertiliser
leaf_aa2 <- leaf_aa[,-c(1)]
leaf_aa3 <- leaf_aa2[,-c(8,17)]
leaf_aa4 <- no_control[,c(3,4,38:55)]
leaf_aa5 <- leaf_aa4[,-c(10,19)]
leaf_aa5 <- na.omit(leaf_aa5)
leaf_aa6 <- leaf_aa5[,-c(1,2)]


colnames(leaf_aa6) <- new_row_names


leaf.nmds2 <- metaMDS(leaf_aa6)
leaf.nmds2$stress
#stress = 0.095
leaf.nmds2
plot(leaf.nmds)

leaf.scores2 <- as.data.frame(scores(leaf.nmds2, "sites"))
leaf.scores2$site <- rownames(leaf.scores2)
leaf.scores2$leaf_aa_fertiliser <- leaf_aa5$fertiliser
head(leaf.scores2)

leaf.scores2$leaf_aa_fertiliser <- as.factor(leaf.scores2$leaf_aa_fertiliser)

leaf.species2 <- as.data.frame(scores(leaf.nmds2, "species"))
leaf.species2$species <- rownames(leaf.species2)

head(leaf.species2)
new_row_names <- c("Asp", "Glu", "His", "Ser", "Thr", "Gly", "Arg", "Ala", "Tyr", "Gaba", "Met", "Val", "Phe", "Ile", "Leu", "Lys")
rownames(root.species) <- new_row_names

seed <- leaf_aa5$seed

ggplot()+
  geom_polygon(data = hull.data.leaf_fertiliser, aes (x = NMDS1, y = NMDS2, fill = leaf_aa_fertiliser, group = leaf_aa_fertiliser), alpha = 0.5, show.legend = FALSE)+
  geom_text(data = leaf.species2, aes( x = NMDS1, y = NMDS2, label = species), alpha = 1, size = 6)+
  geom_point(data = leaf.scores2, aes( x = NMDS1, y = NMDS2,colour = leaf_aa_fertiliser, shape = seed),size = 4)+
  scale_color_manual(values = my_colours, labels = c("IN", "ON"))+
  scale_fill_manual(values = my_colours, name = "Fertiliser", labels = c("IN", "ON"))+
  scale_shape_manual(values = c(16,17), labels = c("KO", "WT"))+
  coord_equal()+
  labs(x = "Leaf NMDS1", y = "Leaf NMDS2")+
  theme_classic()+
  theme(
    axis.title.x = element_text(size = 20, margin = margin(t = 20)),
    axis.title.y = element_text(size = 20, margin = margin(r = 20), family = "Arial"),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.text = element_text(size = 18, color = "black"),
    text = element_text(family = "Arial", color = "black"),
    legend.title = element_blank(),
    legend.spacing.x = unit(0.5, "cm"),
    legend.position = "top",
    plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))+
  guides(fill = guide_legend(byrow = TRUE), colour = guide_legend(override.aes = list(shape = c(15, 15), size = 8, alpha = 0.5)))


ggsave("nmds_leaf_separate_p1.png",
       height = 21.24, width = 30.41,units='cm')

ggsave("nmds_leaf_separate_p2.png",
       height = 21.24, width = 30.41,units='cm')

#save one version with and one without aa labels?


m_com_leaf2 = as.matrix(leaf_aa6)
m_com_leaf2

ano_leaf2 = anosim(m_com_leaf2, leaf_aa5$seed, distance = "bray", permutations = 9999)
ano_leaf2

#seed
#ANOSIM statistic R = 0.3287
#significance = 0.0027



fertiliser.on_leaf <- leaf.scores2[leaf.scores2$leaf_aa_fertiliser == "on",][chull(leaf.scores2[leaf.scores2$leaf_aa_fertiliser == "on", c("NMDS1", "NMDS2")]),]
fertiliser.in_leaf <- leaf.scores2[leaf.scores2$leaf_aa_fertiliser == "in",][chull(leaf.scores2[leaf.scores2$leaf_aa_fertiliser == "in", c("NMDS1", "NMDS2")]),]


hull.data.leaf_fertiliser <- rbind(fertiliser.in_leaf, fertiliser.on_leaf)
hull.data.rootnew  


# Root - seed and fertiliser ----------------------------------------------

leaf_aa <- weight_2[, c(5, 39:55)]
leaf_aa <- na.omit(leaf_aa)
leaf_aa_fertiliser <- leaf_aa5$fertiliser
leaf_aa2 <- leaf_aa[,-c(1)]
leaf_aa3 <- leaf_aa2[,-c(8,17)]
leaf_aa4 <- no_control[,c(3,4,38:55)]
leaf_aa5 <- leaf_aa4[,-c(10,19)]
leaf_aa5 <- na.omit(leaf_aa5)
leaf_aa6 <- leaf_aa5[,-c(1,2)]

root_aa_new <- no_control[,c(3,4,19:36)]
root_aa_new <- root_aa_new[-c(10,19)]
root_aa_new <- na.omit(root_aa_new)
root_aa_new2 <- root_aa_new[,-c(1,2)]

root_aa_fertiliser <- root_aa_new$fertiliser
nrow(root_aa2)

colnames(root_aa_new2) <- new_row_names


root.nmds2 <- metaMDS(root_aa_new2)
root.nmds2$stress
#stress = 0.076
root.nmds2
plot(root.nmds)

root.scores2 <- as.data.frame(scores(root.nmds2, "sites"))
root.scores2$site <- rownames(root.scores2)
root.scores2$root_aa_fertiliser <- root_aa_new$fertiliser
head(root.scores2)

root.scores2$root_aa_fertiliser <- as.factor(root.scores2$root_aa_fertiliser)

root.species2 <- as.data.frame(scores(root.nmds2, "species"))
root.species2$species <- rownames(root.species2)

head(root.species2)
new_row_names <- c("Asp", "Glu", "His", "Ser", "Thr", "Gly", "Arg", "Ala", "Tyr", "Gaba", "Met", "Val", "Phe", "Ile", "Leu", "Lys")
rownames(root.species) <- new_row_names

seed_root <- root_aa_new$seed

ggplot()+
  geom_polygon(data = hull.data.root_fertiliser, aes (x = NMDS1, y = NMDS2, fill = root_aa_fertiliser, group = root_aa_fertiliser), alpha = 0.5, show.legend = FALSE)+
  geom_text(data = root.species2, aes( x = NMDS1, y = NMDS2, label = species), alpha = 1, size = 6)+
  geom_point(data = root.scores2, aes( x = NMDS1, y = NMDS2,colour = root_aa_fertiliser, shape = seed_root),size = 4)+
  scale_color_manual(values = my_colours, labels = c("IN", "ON"))+
  scale_fill_manual(values = my_colours, name = "Fertiliser", labels = c("IN", "ON"))+
  scale_shape_manual(values = c(16,17), labels = c("KO", "WT"))+
  coord_equal()+
  labs(x = "Root NMDS1", y = "Root NMDS2")+
  theme_classic()+
  theme(
    axis.title.x = element_text(size = 20, margin = margin(t = 20)),
    axis.title.y = element_text(size = 20, margin = margin(r = 20), family = "Arial"),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.text = element_text(size = 18, color = "black"),
    text = element_text(family = "Arial", color = "black"),
    legend.title = element_blank(),
    legend.spacing.x = unit(0.5, "cm"),
    legend.position = "top",
    plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))+
  guides(fill = guide_legend(byrow = TRUE), colour = guide_legend(override.aes = list(shape = c(15, 15), size = 8, alpha = 0.5)))


ggsave("nmds_root_separate_p1.png",
       height = 21.24, width = 30.41,units='cm')

ggsave("nmds_root_separate_p2.png",
       height = 21.24, width = 30.41,units='cm')

#save one version with and one without aa labels?


m_com_leaf2 = as.matrix(leaf_aa6)
m_com_leaf2

ano_leaf2 = anosim(m_com_leaf2, leaf_aa5$seed, distance = "bray", permutations = 9999)
ano_leaf2

#seed
#ANOSIM statistic R = 0.3287
#significance = 0.0027

fertiliser.on_root <- root.scores2[root.scores2$root_aa_fertiliser == "on",][chull(root.scores2[root.scores2$root_aa_fertiliser == "on", c("NMDS1", "NMDS2")]),]
fertiliser.in_root <- root.scores2[root.scores2$root_aa_fertiliser == "in",][chull(root.scores2[root.scores2$root_aa_fertiliser == "in", c("NMDS1", "NMDS2")]),]


hull.data.root_fertiliser <- rbind(fertiliser.in_root, fertiliser.on_root)
hull.data.rootnew  

