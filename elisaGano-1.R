
### Questions
##### 1. Is there a difference in reactivity to the crude extracts between group 1, 2, and 3

##### load dataset
library(xlsx)
elisaGano <- read.xlsx("C:/Users/Felix/Dropbox/Scientific/Mentoring/Frances Vila/Datos crudos ELISA.xlsx", 1)

-------------------
### Data cleaning
#### remove spaces from columns's names
names(elisaGano) <- make.names(names(elisaGano))

#### eliminate any duplicated columns
elisaGano <- elisaGano[, !duplicated(colnames(elisaGano))]

#### rename and select columns
library (dplyr)
elisaGanoGroups <- rename(elisaGano, Group1 = Grupo.1..n.51., Group2 = Grupo.2..n.14., Group3 = Grupo.3..n.22.)
elisaGanoGroups1 <- select (elisaGanoGroups, Group1:Group3)

#### tidy the dataset
library(tidyr)
elisaGanoGroups_long <- gather(elisaGanoGroups, Groups, ELISAOD, Group1:Group3)

-------------------
### Exploratory data analysis
#### histograms to evalaute the distribution of ELISA ODs by group
par(mfrow=c(3, 1))
hist (elisaGanoGroups1$Group1)
hist (elisaGanoGroups1$Group2)
hist (elisaGanoGroups1$Group3)

#### descriptive statistics of the ELISA.OD for each group
library (pander)
pander(summary (elisaGanoGroups1))

#### test for normality for each group
shapiro.test(elisaGanoGroups$Group1)
shapiro.test(elisaGanoGroups$Group2)
shapiro.test(elisaGanoGroups$Group3)

#### boxplot of ELISA ODs per group
library(ggplot2)
elisaplot <- ggplot(elisaGanoGroups_long, aes(x=Groups, y=ELISAOD)) + geom_boxplot() + stat_summary(fun.y=mean, geom="point", shape=4, size=4) + guides(fill=FALSE) + theme(axis.title.x = element_blank()) + labs(y=expression (ELISA [A490]))
                                                                                                                                                                                                                                
print(elisaplot)

----------------------
### Inferential statistical analysis
#### pairwise t-test with Bonferroni correction
attach(elisaGanoGroups_long)
pairwise.t.test(ELISAOD, Groups, p.adj = "bonf")
detach()

----------------------




