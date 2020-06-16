## Load packages ##
library(tidyverse)
library(psych)


##################################
### Import Data for Analysis ###
##################################

all.data <- readRDS("Data/DataForAnalysis.rds") 

# all.data <- readRDS("Data/Bot/BotForAnalysis.rds")

all.subject <- subset(all.data, period == 1)

all.subject <- all.subject[c("player.understanding", "player.age","player.gender", "player.nationality", "player.fieldofstudies", "player.income_rank", "player.risk1", "player.risk2", "player.trust1", "player.trust2", "player.trust3", "player.trust_strangers")]

all.subject$player.age <- as.numeric(all.subject$player.age)
all.subject$player.risk1 <- as.numeric(all.subject$player.risk1)
all.subject$player.risk2 <- as.numeric(all.subject$player.risk2)

all.subject$player.risk1 <- all.subject$player.risk1 - 1
all.subject$player.risk2 <- all.subject$player.risk2 - 1
# rescale

summary(all.subject$player.age)
sd(all.subject$player.age)
summary(all.subject$player.risk1)
sd(all.subject$player.risk1)
summary(all.subject$player.risk2)
sd(all.subject$player.risk2)
# numeric

all.subject.2 <- all.subject[c("player.gender", "player.nationality", "player.fieldofstudies", "player.income_rank", "player.trust1", "player.trust2", "player.trust3", "player.trust_strangers")]
# categories

for (i in 1:8) {
  
print(round(prop.table(table(all.subject.2[i])), digits=4))

}
