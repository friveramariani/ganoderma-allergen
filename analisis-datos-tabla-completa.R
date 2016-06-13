## Questions
#### 1. Is there a relationship between reactivity in the Western blot and reactivity to the commercial allergens?

#### 2. Is there a relationship between reactivity to the crude extract (i.e. in the ELISA) and reactivity in the Wesern Blot?

#### 3. Is there a relationship between reactivity to the crude extract and reactivity to the commercial allergens in the Rast?

------------------------------
## Data cleaning to review reactivity in Western blot 

### load dataset
library (xlsx)

tablafull <- read.xlsx ("C:/Users/Felix/Dropbox/Scientific/Mentoring/Frances Vila/Western blots-Group 1.xlsx", 1, startRow=2)

### examine dataset
str (tablafull)

### subset by elisa, western blots, rast reactions

library (plyr); library (dplyr)

wb <- select (tablafull, Serum, X19.Kda:X81.Kda)

### melt subsetted datasets by serum samples
library (reshape2)

wbmelt <- melt (wb, id = c("Serum")); names (wbmelt) <- c("serum", "polypeptide", "reaction")
wbmelt$reaction <- as.factor(wbmelt$reaction)
	
### create contigency table by reactivity in the Western blot
countswb <- table (wbmelt$reaction, wbmelt$polypeptide)

### Fig. 1: barplot Western blot reactivity
par(mar=c(5.1, 4.1, 4.1, 9.1), xpd=TRUE)

barplot (countswb, main = "Western Blot Bands", col=c("black", "red"), ylim=c(0, 51), las=2)
legend("right", inset=c(-0.30,0), legend=rownames(countswb), col=c("black", "red"), pch=15)

### reset to original settings
par (mar=c(5, 4, 4, 2) + 0.1)

### Fisher exact tests for reactivity in the Western blot 
### among serum samples with at least one positive band
binom.test(2, n=33, p=2/33)$conf.int
binom.test(1, n=33, p=1/33)$conf.int
binom.test(6, n=33, p=6/33)$conf.int
binom.test(8, n=33, p=8/33)$conf.int
binom.test(29, n=33, p=29/33)$conf.int
binom.test(12, n=33, p=1/33)$conf.int
binom.test(32, n=33, p=32/33)$conf.int

-----------------------------
## Data cleaning and analysis related to Question 1

### select serum samples and columns with Rast allergen reactivity, and melt the subsetted dataset
rast <- select (tablafull, Serum, Alternaria.tenuis:Corn)

rastmelt <- melt (rast, id = c("Serum")); names (rastmelt) <- c("serum", "allergen", "reaction")

### change "reaction" column to factor class
rastmelt$reaction <- as.factor (rastmelt$reaction)

### create contigency table by reactivity in the Rast to commercial allergens
countsrast <- table (rastmelt$reaction, rastmelt$allergen)### Fig.2 counts of Rast reactivity

### Fig 2: barplot of contigency table for reactivity to the Rast allergens
#### adjust figure's dimension settings
par(mar=c(10.1, 4.1, 4.1, 9.1), xpd=TRUE)

barplot (countsrast, main = "Rast", col=c("yellow", "black", "red"), ylim=c(0, 51), las=2)
legend("right", inset=c(-0.30,0), legend=rownames(countsrast), col=c("yellow", "black", "red"), pch=15)


### subset dataset with allergens that have 3 NAs or less for the Rast allergens
fewallergens <- select (tablafull, Serum, ELISA.OD, X19.Kda, X24.Kda, X33.Kda, X45.kda, X56.kda, X75.Kda, X81.Kda, Alternaria.tenuis, Aspergillus.fumigatus, Cladosporium.herbarum, Penicillium.notatum, Dermatophagoides.farie, Cat.dander.epithelium, Dog.epithelia)

### subset by Reactive to the 56 kDa and 81 kDa polypeptides
positive56 <- filter(tablafull, X56.kda == "Reactive") %>% select (Alternaria.tenuis, Aspergillus.fumigatus, Cladosporium.herbarum, Penicillium.notatum, Dermatophagoides.farie, Cat.dander.epithelium, Dog.epithelia)

positive81 <- filter(tablafull, X81.Kda == "Reactive") %>% select (Alternaria.tenuis, Aspergillus.fumigatus, Cladosporium.herbarum, Penicillium.notatum, Dermatophagoides.farie, Cat.dander.epithelium, Dog.epithelia)

### tidy positive56 and positive81 datasets
library (tidyr)
tidy56allergens <- gather (positive56, allergen, reaction, Alternaria.tenuis:Dog.epithelia)

tidy81allergens <- gather (positive81, allergen, reaction, Alternaria.tenuis:Dog.epithelia)

### create contigency tables of rast reactivity
countsrast56 <- table (tidy56allergens$reaction, tidy56allergens$allergen)

countsrast81 <- table (tidy81allergens$reaction, tidy81allergens$allergen)

### barplots for Rast reactivity with serum samples positive to the 56 and 81 kDa polypeptides
#### adjust figure's dimmension settings
par(mfrow=c(1,2))
par(mar=c(5.1, 4.1, 4.1, 6), xpd=TRUE)

barplot(countsrast56, main="Positive to 56 kDa band", col=c("yellow", "black", "red"), cex.names=0.5, las=2)
legend("right", inset=c(-.7,0), legend=rownames(countsrast81), col=c("yellow", "black", "red"), cex=0.7, pch=15)

barplot(countsrast81, main="Positive to 81 kDa band", col=c("yellow", "black", "red"), cex.names=0.5, las=2)
legend("right", inset=c(-.7,0), legend=rownames(countsrast81), col=c("yellow", "black", "red"), cex=0.7, pch=15)


### Fisher exact for Rast reactivity with serum samples reactive to the 56 kDA polypeptide
4/27
binom.test(4, n=27, p=4/27)$conf.int

05/28
binom.test(5, n=28, p=5/28)$conf.int

13/27
binom.test(13, n=27, p=13/273)$conf.int

03/27
binom.test(3, n=27, p=03/27)$conf.int

25/28
binom.test(25, n=28, p=25/28)$conf.int

14/27
binom.test(14, n=27, p=14/27)$conf.int

04/28
binom.test(4, n=28, p=04/28)$conf.int

### Fisher exact for Rast reactivity with serum samples reactive to the 81 kDa polypeptide
04/30
binom.test(4, n=30, p=4/30)$conf.int

03/31
binom.test(3, n=31, p=4/31)$conf.int

13/30
binom.test(13, n=30, p=4/30)$conf.int

03/30
binom.test(3, n=30, p=4/30)$conf.int

26/31
binom.test(26, n=31, p=4/31)$conf.int

15/30
binom.test(15, n=30, p=4/30)$conf.int

04/30
binom.test(4, n=30, p=4/30)$conf.int

-----------------------------------------------
## Data cleaning and analysis for Question 2
### select ELISA and WB variables
tablaelisawb <- select (tablafull, Serum, ELISA.OD, X19.Kda, X24.Kda, X33.Kda, X45.kda, X56.kda, X75.Kda, X81.Kda)

### create three group from ELISA ODs
tablaelisawb$ODCAT <- cut(tablaelisawb$ELISA.OD, 3)

### contingency tables for reactivity to polypeptides by ELISA ODs' groups
counts.19 <- table (tablaelisawb$X19.Kda, tablaelisawb$ODCAT)
counts.24 <- table (tablaelisawb$X24.Kda, tablaelisawb$ODCAT)
counts.33 <- table (tablaelisawb$X33.Kda, tablaelisawb$ODCAT)
counts.45 <- table (tablaelisawb$X45.kda, tablaelisawb$ODCAT)
counts.56 <- table (tablaelisawb$X56.kda, tablaelisawb$ODCAT)
counts.75 <- table (tablaelisawb$X75.Kda, tablaelisawb$ODCAT)
counts.81 <- table (tablaelisawb$X81.Kda, tablaelisawb$ODCAT)

### Fig 3: barplots of reactivity to polypeptides by ELISA ODs groups
par(mfrow=c(2,4))
barplot(counts.19, main="19 kDA reactivity", col=c("black", "red")); barplot(counts.24, main="24 kDA reactivity", col=c("black", "red")); barplot(counts.33, main="33 kDA reactivity", col=c("black", "red")); barplot(counts.45, main="45 kDA reactivity", col=c("black", "red")); barplot(counts.56, main="56 kDA reactivity", col=c("black", "red")); barplot(counts.75, main="75 kDA reactivity", col=c("black", "red")); 
barplot(counts.81, main="81 kDA reactivity", col=c("black", "red"))

### logistic regression for relationship between ELISA groups and reactivity with Western Blot
wbfit <- glm (factor(reaction) ~ ODCAT, tablaelisawb, family = "binomial")
summary(wbfit)
------------------------------------
## Data cleaning and analysis for Question 3
### select ELISA OD and Rast allergen variables
tablarastelisa <- select (tablafull, ELISA.OD, Alternaria.tenuis, Aspergillus.fumigatus, Cladosporium.herbarum, Penicillium.notatum, Dermatophagoides.farie, Cat.dander.epithelium, Dog.epithelia)

### create three group from ELISA ODs
tablarastelisa$ODCAT <- cut(tablarastelisa$ELISA.OD, 3)

### tidy the dataset based on Rast allergen
library (tidyr)
tablanew <- gather (tablarastelisa, allergen, reaction, Alternaria.tenuis:Dog.epithelia)

### create subset by ELISA ODs group
elisa.1 <- filter (tablanew, ODCAT == "(0.174,0.404]")
elisa.2 <- filter (tablanew, ODCAT == "(0.404,0.633]")
elisa.3 <- filter (tablanew, ODCAT == "(0.633,0.863]")

### contingency tables for reactivity to polypeptides by ELISA ODs' groups
countselisa.1 <- table(elisa.1$reaction, elisa.1$allergen)
countselisa.2 <- table(elisa.2$reaction, elisa.2$allergen)
countselisa.3 <- table(elisa.3$reaction, elisa.3$allergen)

### barplots of Rast allergen contingency tables by ELISA ODs' groups
par(mfrow=c(1,3))
barplot (countselisa.1, col=c("yellow", "black", "red"), main="ELISA ODs (0.174,0.404]", las=2)
barplot (countselisa.2, col=c("yellow", "black", "red"), main="ELISA ODs (0.404,0.633]", las=2)
barplot (countselisa.3, col=c("yellow", "black", "red"), main="ELISA ODs (0.633,0.863]", las=2)

### logistic regression for relationship between ELISA groups and reactivity with Rast allergens
elisafit <- glm (factor(reaction) ~ ODCAT, tablanew, family = "binomial")
summary(elisafit)