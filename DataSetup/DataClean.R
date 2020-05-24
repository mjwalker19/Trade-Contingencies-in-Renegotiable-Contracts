## Load packages ##
library(tidyverse)
library(plyr)
library(Hmisc)

##################################
### Raw Data Import ###
##################################

# The first step is to get a vector of file names (experiment data collected using oTree software http://www.otree.org/)

main1.file.names <- list.files("Data/Main", full.names = TRUE)

# main1.file.names <- list.files("Data/Bot", full.names = TRUE) ## BOT

data_unmerged <- map(main1.file.names, read_csv)

# Stack and Merge
stack1 <- data_unmerged[c(1:5)] # main
stack1_append <- rbind.fill(stack1)
stack1_append$id <- seq(dim(stack1_append)[1]) # id for merge sort order

stack2 <- data_unmerged[c(6:10)] # intro
stack2_append <- rbind.fill(stack2)

stack3 <- data_unmerged[c(11:15)] # survey
stack3_append <- rbind.fill(stack3)

# Training app data excluded since actions directed

stack4 <- data_unmerged[c(16:20)] # payment
stack4_append <- rbind.fill(stack4)

a <- merge(stack1_append, stack2_append[, c("participant.code", setdiff(colnames(stack2_append),colnames(stack1_append)))], by="participant.code", suffixes = c("",".stack2"))
b <- merge(a, stack3_append[, c("participant.code", setdiff(colnames(stack3_append),colnames(a)))], by="participant.code", suffixes = c("",".stack3"))
c <- merge(b, stack4_append[, c("participant.code", setdiff(colnames(stack4_append),colnames(b)))], by="participant.code", suffixes = c("",".stack4"))
data_merged = c[order(c$id),]

data_merged <- data_merged[ which(data_merged$session.code != "o6baw3oz"), ]
data_merged <- data_merged[ which(data_merged$session.code != "58ihjre4"), ]
# Session ids o6baw3oz and 58ihjre4 excluded due to limited liability problem

glimpse(data_merged)

##################################
### Data Tidying ###
##################################

all.data <- data_merged

str(all.data$session.code)
all.data$session.code <- factor(all.data$session.code)

which( colnames(all.data)=="session.code" )
levels(all.data$session.code)

all.data =  all.data %>%
  mutate(cohort.number = 
           fct_recode(session.code, "1" ="wqzuk95c", "2" = "65cotpm1", "3" = "5mzbgeqx", "4" = "clgy04jm", "5"="3qoi5iuv", "6"="elr942ax", 
                      "1"="0i22x0vi", "2"="hkduj07b", "3" = "2nazzfsf", "4" = "xwsdltki", "5" = "bbouwviu"))

all.data <- all.data %>%
  select(cohort.number, everything())                                    

all.data =  all.data %>%
  mutate(session.number = 
           fct_recode(session.code, "1" ="wqzuk95c", "2" ="65cotpm1", "3" = "5mzbgeqx", "3" = "clgy04jm", "4"="3qoi5iuv", "4"="elr942ax",
                      "1"="0i22x0vi", "1"="hkduj07b", "2" = "2nazzfsf", "2" = "xwsdltki", "3" = "bbouwviu"))

all.data <- all.data %>%
  select(session.number, everything())

all.data$cohort.number <- as.character(all.data$cohort.number)
all.data$cohort.number <- as.numeric(all.data$cohort.number)
all.data$session.number <- as.character(all.data$session.number)
all.data$session.number <- as.numeric(all.data$session.number)

  #' Individual-level Variables

# Drop columns [care with relative position]
which( colnames(all.data)=="participant.label" )
which( colnames(all.data)=="participant.mturk_assignment_id" )
which( colnames(all.data)=="player.show_bid" )
which( colnames(all.data)=="player.show_total" )
which( colnames(all.data)=="session.label" )
which( colnames(all.data)=="session.is_demo" )
which( colnames(all.data)=="player.q1" )
which( colnames(all.data)=="player.q5" )

all.data <- all.data[ -c(3, 5:14, 24:31, 49:54, 56:60) ]

all.data <- rename(all.data, c("player.seller_bid"="bid",
                               "player.seller_quality"="strategy.quality",
                               "player.id_in_group"="role", 
                               "player.is_winner"="winner", 
                               "player.payoff"="round.profit",
                               "player.points_accrual"="running.exclendow",
                               "player.points_total"="running.inclendow",
                               "player.experiment_earnings"="total.earnings"))

# 9 subjects per cohort, 30 rounds
all.data <- mutate(all.data, subject.id = participant.id_in_session + 9*(cohort.number - 1))
all.data <- all.data[ -c(3) ]
all.data <- all.data %>%
  select(subject.id, everything())


all.data$role <- as.character(all.data$role)
all.data$role <- revalue(all.data$role, c("3"="2"))
all.data$role <- factor(all.data$role,
                    levels = c(1,2),
                    labels = c("Buyer", "Seller"))

# ***oTree timeout check field***: critical threshold 2%:
table(all.data$player.timeout)

  #' Group-level Variables

all.data <- rename(all.data, c("group.winning_bid"="bid.low", 
                               "group.losing_bid"="bid.high", 
                               "group.price_floor"="floor",
                               "group.buyer_price"="y", 
                               "group.proposed_profit_share"="ypct",
                               "group.arb_price"="pA",
                               "group.final_price"="p",
                               "group.winning_quality"="product.quality"))

# 3 groups per cohort
all.data <- mutate(all.data, group.period.id = group.id_in_subsession + 3*(cohort.number - 1), 
                   arb.available = ifelse(group.dice_number > 3, TRUE, FALSE))

  #' Cohort-level Variables

all.data <- rename(all.data, c("subsession.round_number"="period", 
                               "subsession.treatment"="treatment", 
                               "subsession.flexibility"="par.lambda", 
                               "subsession.prob_arb"="par.sigma", 
                               "subsession.pref_arb_1"="par.mu1", 
                               "subsession.pref_arb_2"="par.mu2", 
                               "subsession.arb_cost"="par.k", 
                               "subsession.order"="order"))

all.data$order <- as.character(all.data$order)

all.data$order <- factor(all.data$order,
                        levels = c(12,21),
                        labels = c("12", "21"))


all.data$treatment <- factor(all.data$treatment,
                        levels = c(1,2),
                        labels = c("Voluntary", "Arbitrator"))


  #' Post-experiment questionnaire

all.data$player.gender <- factor(all.data$player.gender,
                                levels = c(0,1,2,3),
                                labels = c("Male", "Female", "Other", "Prefer not to say"))

all.data$player.nationality <- factor(all.data$player.nationality,
                                 levels = c(1,2,3,4,5,6,7,8,9,10,11),
                                 labels = c("Central and Eastern Asia", "Central and Western Africa", "Central, South America and the Caribbean", "Europe (excl. UK)", "Middle East and North Africa", "North America", "Oceania", "South and Eastern Africa", "South-East Asia", "Southern Asia", "UK"))

all.data$player.fieldofstudies <- factor(all.data$player.fieldofstudies,
                                      levels = c(1,2,3,4,5,6,7),
                                      labels = c("Arts and Education", "Economics and Finance", "Business and Management", "Engineering and Natural Sciences", "Law and Social Sciences", "Medicine and Health Sciences", "Not a Student"))


all.data <- mutate(all.data, econ = ifelse(as.character(all.data$player.fieldofstudies == "Economics and Finance"), 1, 0))
#select(all.data, player.fieldofstudies, econ)

all.data$player.income_rank <- factor(all.data$player.income_rank, ordered = TRUE,
                                 levels = c(1,2,3,4,5),
                                 labels = c("Far below average", "Below average", "Average", "Above average", "Far above average"))

all.data$player.trust1 <- as.numeric(all.data$player.trust1)
all.data$player.trust2 <- as.numeric(all.data$player.trust2)
all.data$player.trust3 <- as.numeric(all.data$player.trust3)

all.data$player.risk1 <- as.numeric(all.data$player.risk1)
all.data$player.risk2 <- as.numeric(all.data$player.risk2)

all.data <- mutate(all.data, player.trust.index = (player.trust1 + player.trust2 + player.trust3)/3)

all.data$player.trust1 <- factor(all.data$player.trust1,
                                 levels = c(0,1),
                                 labels = c("Can’t be too careful", "Most people can be trusted"))

all.data$player.trust2 <- factor(all.data$player.trust2,
                                 levels = c(0,1),
                                 labels = c("Would take advantage of you", "Would try to be fair"))

all.data$player.trust3 <- factor(all.data$player.trust3,
                                 levels = c(0,1),
                                 labels = c("Just look out for themselves", "Try to be helpful"))

all.data$player.trust_strangers <- factor(all.data$player.trust_strangers,
                                 levels = c(0,1),
                                 labels = c("More or less agree", "More or less disagree"))

which( colnames(all.data)=="player.risk1" )

all.data[,c(53:54)] <- lapply(all.data[,c(53:54)], factor, ordered = TRUE, 
                      levels=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
                      labels = c("completely unwilling to take risks", "1", "2", "3", "4", "5", "6", "7", "8", "9",
                                 "completely willing to take risks"))

str(all.data)

label(all.data$player.understanding) <- "How would you rate your understanding of the experiment?" 
label(all.data$player.age) <- "What is your age" 
label(all.data$player.gender) <-"What is your gender" 
label(all.data$player.nationality) <-"What is your nationality?" 
label(all.data$player.fieldofstudies) <-"If you are a student, what is your subject?" 
label(all.data$player.income_rank) <-"When you were 16 years of age, what was the income of your parents in comparison to other families in your country?" 

label(all.data$player.trust1) <-"Generally speaking, would you say that most people can be trusted or that you can’t be too careful in dealing with people?" 
label(all.data$player.trust2) <-"Do you think most people would try to take advantage of you if they got a chance, or would they try to be fair?" 
label(all.data$player.trust3) <-"Would you say that most of the time people try to be helpful, or that they are mostly just looking out for themselves?" 
label(all.data$player.trust_strangers) <-"You can’t count on strangers anymore." 

label(all.data$player.risk1) <- "Are you generally a person who is fully willing to take risks or do you try to avoid taking risks?"
label(all.data$player.risk2) <- "How would you rate your willingness to take risks in financial matters?"

all.data <- mutate(all.data, sub = ifelse(treatment == "Arbitrator", ifelse((order == "12" & period < 16)|(order == "21" & period > 15), "mu1", "mu2"), 0))
      
all.data$sub <- as.character(all.data$sub)
all.data$sub <- factor(all.data$sub)
                                         
which( colnames(all.data)=="cohort.number" )
which( colnames(all.data)=="period" )

all.data <- subset(all.data, select=c(1:3,28,4:27,29:60))

which( colnames(all.data)=="group.dice_number" )
which( colnames(all.data)=="arb.available" )

all.data <- subset(all.data, select=c(1:24,57,25:56,58:60))

which( colnames(all.data)=="treatment" )

all.data <- subset(all.data, select=c(33, 1:32, 34:60))

which( colnames(all.data)=="sub" )

all.data <- subset(all.data, select=c(1, 60, 2:59))

which( colnames(all.data)=="order" )

all.data <- subset(all.data, select=c(1:2, 35, 3:34, 36:60))

all.data$par.mu <- ifelse(all.data$sub == "mu1", 1/3,  ifelse(all.data$sub == "mu2", 2/3,  NA))

which( colnames(all.data)=="par.mu" )

all.data <- subset(all.data, select=c(1:2, 61, 3:60))

##################################
### Data Quality Screening ###
##################################

## Data lost due to Timeouts ##

all.data <- mutate(all.data, lost.trade.data = 
                     ifelse((player.timeout == 1 & winner == 1)|
                              (player.timeout == 1 & role == "Buyer"), 1, 0), # captures buyer timeout and both sellers timing out [latter never occurred]
                   lost.auction.data = ifelse(role == "Seller" & player.timeout == 1, 1, 0))

# Voluntary

timeout.data <- subset(all.data, treatment == "Voluntary" & lost.trade.data == 1)

to.period.vector <- timeout.data[['period']]
to.group.period.id.vector <- timeout.data[['group.period.id']]

vals <- seq(1, length(to.period.vector), 1)

for (i in vals) {
  
  all.data$lost.trade.data <- ifelse(all.data$treatment == "Voluntary" & all.data$period == to.period.vector[i] & all.data$group.period.id == to.group.period.id.vector[i], 1, all.data$lost.trade.data)
  
}

timeout.data <- subset(all.data, treatment == "Voluntary" & lost.trade.data == 1) # CHECK = 3 X LENGTH OF to.period.vector

# Arbitrator

timeout.data <- subset(all.data, treatment == "Arbitrator" & lost.trade.data == 1)

to.period.vector <- timeout.data[['period']]
to.group.period.id.vector <- timeout.data[['group.period.id']]

vals <- seq(1, length(to.period.vector), 1)

for (i in vals) {
  
  all.data$lost.trade.data <- ifelse(all.data$treatment == "Arbitrator" & all.data$period == to.period.vector[i] & all.data$group.period.id == to.group.period.id.vector[i], 1, all.data$lost.trade.data)
  
}

timeout.data <- subset(all.data, treatment == "Arbitrator" & lost.trade.data == 1) # CHECK = 3 X LENGTH OF to.period.vector

## Limited Liability Check ##

summary(all.data$player.points_final)
# If limited liability a problem follow additional exclusion measures in script LLexclusion.R

### NA Lost auction/trade data

# Auction
all.data$bid <- ifelse(all.data$lost.auction.data == 1, NA, all.data$bid)
all.data$strategy.quality <- ifelse(all.data$lost.auction.data == 1, NA, all.data$strategy.quality)

# Trade
all.data$round.profit <- ifelse(all.data$lost.trade.data == 1, NA, all.data$round.profit)
all.data$floor <- ifelse(all.data$lost.trade.data == 1, NA, all.data$floor)
all.data$product.quality <- ifelse(all.data$lost.trade.data == 1, NA, all.data$product.quality)
all.data$y <- ifelse(all.data$lost.trade.data == 1, NA, all.data$y)
all.data$ypct <- ifelse(all.data$lost.trade.data == 1, NA, all.data$ypct)
all.data$pA <- ifelse(all.data$lost.trade.data == 1, NA, all.data$pA)
all.data$group.seller_appeal <- ifelse(all.data$lost.trade.data == 1, NA, all.data$group.seller_appeal)
all.data$group.dice_number <- ifelse(all.data$lost.trade.data == 1, NA, all.data$group.dice_number)
all.data$arb.available <- ifelse(all.data$lost.trade.data == 1, NA, all.data$arb.available)
all.data$group.buyer_arb_cost <- ifelse(all.data$lost.trade.data == 1, NA, all.data$group.buyer_arb_cost)
all.data$p <- ifelse(all.data$lost.trade.data == 1, NA, all.data$p)
all.data$group.buyer_payoff <- ifelse(all.data$lost.trade.data == 1, NA, all.data$group.buyer_payoff)
all.data$group.seller_payoff <- ifelse(all.data$lost.trade.data == 1, NA, all.data$group.seller_payoff)

##################################
### Save Intermediate Dataset for Analysis ###
##################################

write_rds(all.data, "Data/DataForAnalysis.rds")

# write_rds(all.data, "Data/Bot/BotForAnalysis.rds")

