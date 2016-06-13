
### Questions considered prior to this exploratory data analysis

#### 1. Is there an association between the serum's reactivity in the Western Blot and their reactivity against the whole extract tested with the ELISA?

#### 2. Does the 95% confidence intervals of the proportion of reactive:non-reactive differs between the bands tested in the Western Blot?

-------------------------------
### Loading and evaluate the Dataset
library(xlsx)
Ganoderma_allergen <- read.xlsx("C:/Users/Felix/Dropbox/Scientific/Mentoring/Frances Vila/Western blots-Group 1.xlsx", 1)

str(Ganoderma_allergen)

### Names of the 39 columns
names (Ganoderma_allergen)

### Descriptive analysis of the whole dataset
summary(Ganoderma_allergen)
-------------------------------
### Data cleaning for exploratory analysis related to Question 2
#### Subset of data (values of the ELISA and Western Blot for each serum sample)

#### select columns for ELISA.OD and columns reporting reactivity to polypeptides
library (dplyr)
Ganoderma.ELISA.WB <- select(Ganoderma_allergen, ELISA.OD:X81.Kda)

#### Group Western Blot values per how many reactive per serum samples

#### Convert Non-Reactive = 0
Ganoderma.ELISA.WB$X19.Kda <- gsub("^Non-Reactive$", "0", Ganoderma.ELISA.WB$X19.Kda)
Ganoderma.ELISA.WB$X24.Kda <- gsub("^Non-Reactive$", "0", Ganoderma.ELISA.WB$X24.Kda)
Ganoderma.ELISA.WB$X33.Kda <- gsub("^Non-Reactive$", "0", Ganoderma.ELISA.WB$X33.Kda)
Ganoderma.ELISA.WB$X45.kda <- gsub("^Non-Reactive$", "0", Ganoderma.ELISA.WB$X45.kda)
Ganoderma.ELISA.WB$X56.kda <- gsub("^Non-Reactive$", "0", Ganoderma.ELISA.WB$X56.kda)
Ganoderma.ELISA.WB$X75.Kda <- gsub("^Non-Reactive$", "0", Ganoderma.ELISA.WB$X75.Kda)
Ganoderma.ELISA.WB$X81.Kda <- gsub("^Non-Reactive$", "0", Ganoderma.ELISA.WB$X81.Kda)

#### Convert Reactive = 1
Ganoderma.ELISA.WB$X19.Kda <- gsub("^Reactive$", "1", Ganoderma.ELISA.WB$X19.Kda)
Ganoderma.ELISA.WB$X24.Kda <- gsub("^Reactive$", "1", Ganoderma.ELISA.WB$X24.Kda)
Ganoderma.ELISA.WB$X33.Kda <- gsub("^Reactive$", "1", Ganoderma.ELISA.WB$X33.Kda)
Ganoderma.ELISA.WB$X45.kda <- gsub("^Reactive$", "1", Ganoderma.ELISA.WB$X45.kda)
Ganoderma.ELISA.WB$X56.kda <- gsub("^Reactive$", "1", Ganoderma.ELISA.WB$X56.kda)
Ganoderma.ELISA.WB$X75.Kda <- gsub("^Reactive$", "1", Ganoderma.ELISA.WB$X75.Kda)
Ganoderma.ELISA.WB$X81.Kda <- gsub("^Reactive$", "1", Ganoderma.ELISA.WB$X81.Kda)

#### Convert 0's and 1's from character to numeric
Ganoderma.ELISA.WB$X19.Kda <- as.numeric (Ganoderma.ELISA.WB$X19.Kda)
Ganoderma.ELISA.WB$X24.Kda <- as.numeric (Ganoderma.ELISA.WB$X24.Kda)
Ganoderma.ELISA.WB$X33.Kda <- as.numeric (Ganoderma.ELISA.WB$X33.Kda)
Ganoderma.ELISA.WB$X45.kda <- as.numeric (Ganoderma.ELISA.WB$X45.kda)
Ganoderma.ELISA.WB$X56.kda <- as.numeric (Ganoderma.ELISA.WB$X56.kda)
Ganoderma.ELISA.WB$X75.Kda <- as.numeric (Ganoderma.ELISA.WB$X75.Kda)
Ganoderma.ELISA.WB$X81.Kda <- as.numeric (Ganoderma.ELISA.WB$X81.Kda)

#### Create column with the sum of reactives per serum sample
Ganoderma.ELISA.WB <- mutate(Ganoderma.ELISA.WB, Sum.Reactives = X19.Kda + X24.Kda +  X33.Kda + X45.kda + X56.kda + X75.Kda + X81.Kda)

-------------------------------------
### Plots for exploratory analysis related to Question 2
#### Histogram of Sum.Reactives column
hist(Ganoderma.ELISA.WB$Sum.Reactives, ylim = c(0, 20), xlab = "Sum of Reactive Western Blot bands per Serum Sample", main=NULL)

#### Change values in Sum.Reactives column from numeric to factor
Ganoderma.ELISA.WB$Sum.Reactives <- as.factor(Ganoderma.ELISA.WB$Sum.Reactives)

#### Boxplot of Sum of Reactives vs ELISA OD's
plot(Ganoderma.ELISA.WB$Sum.Reactives, Ganoderma.ELISA.WB$ELISA.OD, xlab = "Total of Reactive Western Blot Bands per Serum", ylab = "ELISA OD", ylim = c(0.1, 1.0))
abline(h=mean (Ganoderma.ELISA.WB$ELISA.OD), col="red")

------------------------------------
### Analysis related to Question 3

summary (select(Ganoderma_allergen, X19.Kda:X81.Kda))

binom.test(2, n=33, p=2/33)$conf.int
binom.test(1, n=33, p=1/33)$conf.int
binom.test(6, n=33, p=6/33)$conf.int
binom.test(8, n=33, p=8/33)$conf.int
binom.test(29, n=33, p=29/33)$conf.int
binom.test(12, n=33, p=1/33)$conf.int
binom.test(32, n=33, p=32/33)$conf.int

#### Table of proportion of Reactives per Western Blond band
Western Blot Band  | % Reactive    | 95% CI Reactive  
------------------ | ----------    | ---------------
19 kDa             | 2/33  =  6.0% |   0.7  -  20.2%
24 kDa             | 1/33  =  3.0% |   0.08 -  15.8% 
33 kDa             | 6/33  = 18.0% |   7.0  -  35.5%    
45 kDa             | 8/33  = 24.0% |   11.1 -  42.3%
56 kDa             | 29/33 = 88.0% |**71.8  -  96.6%** 
75 kDa             | 1/33  =  3.0% |   0.08 -  15.8%
81 kDa             | 32/51 = 97.0% |**84.2  -  99.9%**


#### A column was created with two levels: 1 = Reactive to any Western Blot band; 0 = Non-Reactive to all the Western Blot Bands
Ganoderma.ELISA.WB <- mutate (Ganoderma.ELISA.WB, Reactivity = ifelse (Sum.Reactives == 0, 0, ifelse (Sum.Reactives !=0, 1, 1)))
Ganoderma.ELISA.WB$Reactivity <- as.factor (Ganoderma.ELISA.WB$Reactivity)

#### boxplot of ELISA.OD of reactive (1) vs non-reactive (0)
plot (Ganoderma.ELISA.WB$Reactivity, Ganoderma.ELISA.WB$ELISA.OD, xlab = "Western Blot Reactivity (1 = Reactive, 0 = Non-Reactive)", ylab = "ELISA OD", ylim = c(0.1, 1.0))
abline(h=mean (Ganoderma.ELISA.WB$ELISA.OD), col="red")

#### unpaired t-test of ELISA between reactive vs non-reactive
library (pander)
pander (t.test (ELISA.OD ~ Reactivity, data= Ganoderma.ELISA.WB, paired = FALSE))

 