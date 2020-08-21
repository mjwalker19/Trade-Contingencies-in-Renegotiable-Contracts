## Load packages ##
library(tidyverse)
library(gganimate)
library(plyr)
library(psych)
library(Hmisc)
library(Rmisc)
library(ggpubr)
library(broom)
library(boot) # for Nonparametric Bootstrap Confidence Intervals
library(pastecs) # for trend.test
library(lmtest)
library(plm)
library(car)
# applies to fit/test <- lm, glm, nls, t.test, cor.test, and wilcox.test.
# tidy(reg)
# augment(reg)
# glance(reg)

##################################
### Prepare Data for Analysis ###
##################################

all.data <- readRDS("Data/DataForAnalysis.rds") 

# all.data <- readRDS("Data/Bot/BotForAnalysis.rds")

## Additional variables for analysis ##

all.data <- mutate(all.data, losing.quality = ifelse(winner == 0, strategy.quality, NA), 
                   proposal.ratio = y / bid.low)

all.data <- mutate(all.data, proposal.ratio.high = ifelse(product.quality == 1, proposal.ratio, NA),
                   proposal.ratio.low = ifelse(product.quality == 0, proposal.ratio, NA), 
                   y.high = ifelse(product.quality == 1, y, NA),
                   y.low = ifelse(product.quality == 0, y, NA),
                   ypct.high = ifelse(product.quality == 1, ypct, NA),
                   ypct.low = ifelse(product.quality == 0, ypct, NA),
                   p.high = ifelse(product.quality == 1, p, NA),
                   p.low = ifelse(product.quality == 0, p, NA),
                   seller_appeal.high = ifelse(product.quality == 1, group.seller_appeal, NA),
                   seller_appeal.low = ifelse(product.quality == 0, group.seller_appeal, NA),
                   buyer_payoff.high = ifelse(product.quality == 1, group.buyer_payoff, NA),
                   buyer_payoff.low = ifelse(product.quality == 0, group.buyer_payoff, NA),
                   seller_payoff.high = ifelse(product.quality == 1, group.seller_payoff, NA),
                   seller_payoff.low = ifelse(product.quality == 0, group.seller_payoff, NA), 
                   realized.surplus = group.buyer_payoff + group.seller_payoff
                   )

all.data <- mutate(all.data, efficiency = realized.surplus / 60)

# Price floor check (>=0.25 ratio)

trade.data <- subset(all.data, lost.trade.data == 0)

summary(trade.data$proposal.ratio)

# Create dataframes by treatment

treatments <- c("Voluntary", "Arbitrator")

for (val in treatments) {
  
  assign(paste0(val), filter(all.data, treatment == val))
  
}

list.treatment <- list(Voluntary, Arbitrator)
names(list.treatment) <- c("Voluntary", "Arbitrator")

labs1 <- c("Voluntary", "Arbitrator: \u03bc = 1/3", "Arbitrator: \u03bc = 2/3")
names(labs1) <- c("Voluntary", "mu1", "mu2")

Cohort.Main <- aggregate(all.data[c("bid", "strategy.quality", "bid.low", "product.quality", "proposal.ratio", "proposal.ratio.low", "proposal.ratio.high", "ypct", "ypct.low", "ypct.high", "group.seller_appeal", "seller_appeal.low", "seller_appeal.high", "p", "p.low", "p.high", "group.buyer_payoff", "buyer_payoff.low", "buyer_payoff.high", "group.seller_payoff", "seller_payoff.low", "seller_payoff.high", "efficiency")],
                         by=list(all.data$treatment, all.data$cohort.number),
                         FUN=mean, na.rm=TRUE)

Cohort.Main <- rename(Cohort.Main, c("Group.1"="treatment", "Group.2"="cohort.number"))

Cohort.Sub <- aggregate(Arbitrator[c("bid", "strategy.quality", "bid.low", "product.quality", "proposal.ratio", "proposal.ratio.low", "proposal.ratio.high", "ypct", "ypct.low", "ypct.high", "group.seller_appeal", "seller_appeal.low", "seller_appeal.high", "p", "p.low", "p.high", "group.buyer_payoff", "buyer_payoff.low", "buyer_payoff.high", "group.seller_payoff", "seller_payoff.low", "seller_payoff.high", "efficiency")],
                        by=list(Arbitrator$sub, Arbitrator$order, Arbitrator$cohort.number),
                        FUN=mean, na.rm=TRUE)

Cohort.Sub <- rename(Cohort.Sub, c("Group.1"="sub", "Group.2"="order", "Group.3"="cohort.number"))

all.data <- mutate(all.data, condition = ifelse(sub=="0", "Voluntary",
                                                ifelse(sub=="mu1", "mu1", "mu2")))


all.data$condition <- as.factor(all.data$condition)
all.data$condition <- relevel(all.data$condition, ref = "Voluntary")

all.data$bid.rescale <- all.data$bid/100
all.data$bid.low.rescale <- all.data$bid.low/100

###############################################################
### Each of the following sections can be run independently ###
###############################################################

##################################
### Procedure ###
##################################

# Average subject payments for protocol (by treatment)

summary(subset(all.data$total.earnings, all.data$role == "Buyer"))
summary(subset(all.data$total.earnings, all.data$role == "Seller"))

summary(subset(all.data$total.earnings, (all.data$role == "Buyer" & all.data$treatment == "Voluntary")))
summary(subset(all.data$total.earnings, (all.data$role == "Buyer" & all.data$treatment == "Arbitrator")))
summary(subset(all.data$total.earnings, (all.data$role == "Seller" & all.data$treatment == "Voluntary")))
summary(subset(all.data$total.earnings, (all.data$role == "Seller" & all.data$treatment == "Arbitrator")))

##################################
### Aggregate Findings ###
##################################

  ## Auctions

auction.data.BS <- subset(Voluntary, role == "Seller" & lost.auction.data == 0)
# no. obsv = no. auctions

auction.data.Arb <- subset(Arbitrator, role == "Seller" & lost.auction.data == 0)
# no. obsv = no. auctions

  ## Trade

trade.data <- aggregate(Voluntary[c("cohort.number", "lost.trade.data")],
                                   by=list(Voluntary$period, Voluntary$group.period.id),
                                   FUN=mean, na.rm=TRUE)

trade.data.BS <- subset(trade.data, lost.trade.data == 0)
# no. obsv = no. trade relationships

trade.data <- aggregate(Arbitrator[c("cohort.number", "lost.trade.data")],
                        by=list(Arbitrator$period, Arbitrator$group.period.id),
                        FUN=mean, na.rm=TRUE)

trade.data.Arb <- subset(trade.data, lost.trade.data == 0)
# no. obsv = no. trade relationships

# Auction and trade outcomes (by cohort)

des.Cohort.Main <- describeBy(Cohort.Main, Cohort.Main$treatment,
                      mat=TRUE,digits=2)  #matrix output
des.Cohort.Main <- rename(des.Cohort.Main, c("group1"="treatment"))
des.Cohort.Main <- subset(des.Cohort.Main, select=c(2, 7))

des.Cohort.Main <- des.Cohort.Main[-c(1:4, 13:18), ] # remove for summ stat table
# des.Cohort.Main[,2] <- paste0("(", format(unlist(des.Cohort.Main[,2])),")") # for SD formatting if switch to Mean (SD)

write_csv(des.Cohort.Main, "DataAnalysis/SummStatMain.csv")
## Custom format SD cells in Excel #,##0.00;(#,##0.00)

Cohort.Sub$Sub <- as.factor(ifelse(Cohort.Sub$sub == "mu1", "1/3", "2/3"))

des.Cohort.Sub <- describeBy(Cohort.Sub, Cohort.Sub$Sub,
                              mat=TRUE,digits=2)  #matrix output

des.Cohort.Sub <- rename(des.Cohort.Sub, c("group1"="Sub"))
des.Cohort.Sub <- subset(des.Cohort.Sub, select=c(2, 7))

des.Cohort.Sub <- des.Cohort.Sub[-c(1:6, 15:20, 53:54), ] # remove for summ stat table
# des.Cohort.Sub[,2] <- paste0("(", format(unlist(des.Cohort.Sub[,2])),")") # for SD formatting if switch to Mean (SD)

write_csv(des.Cohort.Sub, "DataAnalysis/SummStatArb.csv")
## Custom format SD cells in Excel #,##0.00;(#,##0.00)

##################################
### Graphical Representations ###
##################################

# A. Winning bids and trade efficiency over time.
# *2 axes plot
ts.data <- all.data
ts.data$condition <- as.factor(ifelse(all.data$sub == "0", "Voluntary", ifelse(all.data$order == "12", "Arbitrator-LH", "Arbitrator-HL")))
ts.data <- aggregate(ts.data[c("bid.low", "floor", "product.quality", "y", "y.high", "y.low", "ypct", "ypct.high", "ypct.low", "par.mu1", "par.mu2", "efficiency")],
                         by=list(ts.data$condition, ts.data$period),
                         FUN=mean, na.rm=TRUE)
ts.data$ypct <- round(ts.data$ypct, digits=4)
ts.data$ypct.high <- round(ts.data$ypct.high, digits=4)
ts.data$ypct.low <- round(ts.data$ypct.low, digits=4)

ts.data <- rename(ts.data, c("Group.1"="treatment", "Group.2"="period"))

ts.data$treatment <- relevel(ts.data$treatment, ref = "Arbitrator-LH")
ts.data$treatment <- relevel(ts.data$treatment, ref = "Voluntary")
ts.data$treatment

ts.data <- mutate(ts.data, bid.theory.vol = ifelse(treatment == "Voluntary", 1.2, NA))
ts.data <- mutate(ts.data, bid.theory.mu1 = ifelse((treatment == "Arbitrator-LH" & period<16)|(treatment == "Arbitrator-HL" & period>15), 0.8, NA))
ts.data <- mutate(ts.data, bid.theory.mu2 = ifelse((treatment == "Arbitrator-HL" & period<16)|(treatment == "Arbitrator-LH" & period>15), 0.64, NA))

ts.data <- mutate(ts.data, change = ifelse(treatment=="Voluntary", NA, 16))

ts.data.rec <- ts.data
ts.data.rec$bid.low.rescale <- ts.data.rec$bid.low/100
ts.data.rec$floor.rescale <- ts.data.rec$floor/100
ts.data.rec$y.rescale <- ts.data.rec$y/100
ts.data.rec$y.high.rescale <- ts.data.rec$y.high/100
ts.data.rec$y.low.rescale <- ts.data.rec$y.low/100
ts.data.rec$product.quality.rescale <- ts.data.rec$product.quality+0.5
ts.data.rec$efficiency.rescale <- ts.data.rec$efficiency+0.5

graph_A <- gather(ts.data.rec,
                    value = "value",
                    key = "type",
                    bid.low.rescale, bid.theory.vol, efficiency.rescale)

png(filename = "DataAnalysis/A.ts.bid.low.eff.png",height=800,width=800)  

p <- ggplot(graph_A, aes(x = period)) +
  facet_wrap(treatment~., nrow=3)+
  geom_line(aes(linetype = type, y=value), size=1) +
  geom_vline(aes(xintercept = change), linetype = "dashed") +
  geom_line(aes(y = bid.theory.mu1), linetype = "dotted", size=1) +
  geom_line(aes(y = bid.theory.mu2), linetype = "dotted", size=1) +
  scale_linetype_manual(name = "", values = c("solid","dotted","dotdash"), labels=c("Average winning bid (left axis)", 
                                                                                    "Competitive bid level (left axis)",
                                                                                    "Efficiency (right axis)")) +
  scale_y_continuous(name = "Price (normalized / 100) \n", limits = c(0.5,1.5), 
                     sec.axis = sec_axis(~.-0.5, name = "Realized proportion of surplus \n")) + 
  scale_x_continuous(name = "Period", breaks=c(1,5,10,15,20,25,30), labels=c(1,5,10,15,20,25,30)) +
  guides(linetype = guide_legend(override.aes = list(size = 1))) +
  theme_bw() +
  theme(
    strip.text.x = element_text(size = 18),
    strip.text.y = element_text(size = 18),
    panel.border = element_blank(),
        panel.grid.major.y = element_line(color = "grey"),
        text = element_text(color = "gray20", size = 16),
        axis.title.x = element_text(face="italic"),
        axis.title.y = element_text(face="italic"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "vertical",
        legend.title=element_text(size=14),
        legend.text = element_text(size = 16),
        legend.spacing.x = unit(1.0, 'cm'),
        plot.caption = element_text(hjust=0, size = 12),
        plot.title = element_text(size = 16, face = "bold"))
p

dev.off()

# B. Time series contract price range and buyer reciprocity.
# y

graph_B <- gather(ts.data.rec,
                  value = "value",
                  key = "type",
                  bid.low.rescale, floor.rescale, y.high.rescale, y.low.rescale)

png(filename = "DataAnalysis/B.ts.range.proposal.png",height=800,width=800)  

p <- ggplot(graph_B, aes(x = period, y=value)) +
  facet_wrap(treatment~., nrow=3)+
  geom_line(aes(linetype = type), size=1) +
  geom_vline(aes(xintercept = change), linetype = "dashed") +
  scale_linetype_manual(name = "", values = c("solid","solid", "dotdash", "dotted"), labels=c("Average winning bid", 
                                                                                     "Average lower bound price",
                                                                                     "Average buyer proposal high quality", 
                                                                                     "Average buyer proposal low quality")) +
  scale_y_continuous(name = "Price (normalized / 100) \n", limits = c(0,1.2)) + # adjust to space covered 
  scale_x_continuous(name = "Period", breaks=c(1,5,10,15,20,25,30), labels=c(1,5,10,15,20,25,30)) +
  guides(linetype = guide_legend(override.aes = list(size = 1), nrow = 2)) +
  theme_bw() +
  theme(
    strip.text.x = element_text(size = 18),
    strip.text.y = element_text(size = 18),
    panel.border = element_blank(),
        panel.grid.major.y = element_line(color = "grey"),
        text = element_text(color = "gray20", size = 16),
        axis.title.x = element_text(face="italic"),
        axis.title.y = element_text(face="italic"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "vertical",
        legend.title=element_text(size=14),
        legend.text = element_text(size = 16),
        legend.spacing.x = unit(1.0, 'cm'),
        plot.caption = element_text(hjust=0, size = 12),
        plot.title = element_text(size = 16, face = "bold"))
p

dev.off()

# C. Time series buyer proposals and trade surplus.
# y.pct

ts.data.rec$mu <- ifelse(ts.data.rec$condition == "mu1", ts.data.rec$par.mu1, ifelse(ts.data.rec$condition == "mu2", ts.data.rec$par.mu2, NA))

graph_C <- gather(ts.data.rec,
                  value = "value",
                  key = "type",
                  mu, ypct.high, ypct.low)

png(filename = "DataAnalysis/C.ts.proposal.surplus.png",height=800,width=800)  

p <- ggplot(graph_C, aes(x = period, y=value)) +
  facet_wrap(condition~., nrow=3, labeller = labeller(condition = labs1))+
  geom_line(aes(linetype = type), size=1) +
  scale_linetype_manual(name = "", values = c("solid", "dotted", "dotdash"), labels=c("Arbitrator's preference parameter", 
                                                                                      "Proposed seller share high quality", 
                                                                                     "Proposed seller share low quality")) +
  
  scale_y_continuous(name = "Proportion of trade surplus \n", limits = c(-0.5,0.5)) + # UPDATE BOUNDS WITH REAL DATA
  
  scale_x_continuous(name = "Period", breaks=c(1,5,10,15,20,25,30), labels=c(1,5,10,15,20,25,30)) +
  guides(linetype = guide_legend(override.aes = list(size = 1))) +
  theme_bw() +
  theme(
        strip.text.x = element_text(size = 18),
        strip.text.y = element_text(size = 18),
        panel.border = element_blank(),
        panel.grid.major.y = element_line(color = "grey"),
        text = element_text(color = "gray20", size = 16),
        axis.title.x = element_text(face="italic"),
        axis.title.y = element_text(face="italic"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "vertical",
        legend.title=element_text(size=14),
        legend.text = element_text(size = 16),
        legend.spacing.x = unit(1.0, 'cm'),
        plot.caption = element_text(hjust=0, size = 12),
        plot.title = element_text(size = 16, face = "bold"))
p

dev.off()

# D. Price distribution and the relative frequency of high product quality.

rel.freq.data <- aggregate(all.data[c("bid.low", "product.quality", "group.seller_appeal")],
                                        by=list(all.data$treatment, all.data$sub, all.data$cohort.number, all.data$period, all.data$group.period.id), FUN=mean, na.rm=TRUE)

rel.freq.data <- rename(rel.freq.data, c("Group.1"="treatment", "Group.2"="sub", "Group.3"="cohort.number", "Group.4"="period", "Group.5"="group.period.id"))

d1 <- aggregate(rel.freq.data[c("product.quality", "group.seller_appeal")], by=list(rel.freq.data$sub, rel.freq.data$bid.low),
                FUN=mean, na.rm=TRUE)

rel.freq.data$trade <- ifelse(!is.na(rel.freq.data$bid.low), 1, 0)

d2 <- aggregate(rel.freq.data[c("trade")], by=list(rel.freq.data$sub, rel.freq.data$bid.low),
                FUN=sum, na.rm=TRUE)

d3 <- aggregate(rel.freq.data[c("trade")], by=list(rel.freq.data$sub),
                FUN=sum, na.rm=TRUE)

d3 <- rename(d3, c("trade"="total"))

rel.freq.data <- merge(d1, d2)

rel.freq.data <- join(rel.freq.data, d3)

rel.freq.data <- rename(rel.freq.data, c("Group.1"="sub", "Group.2"="bid.low"))

graph_D <- rel.freq.data

graph_D <- mutate(graph_D, condition = ifelse(sub=="0", "Voluntary",
                                                          ifelse(sub=="mu1", "mu1", "mu2")))

graph_D$condition <- as.factor(graph_D$condition)
graph_D$condition <- relevel(graph_D$condition, ref = "Voluntary")
graph_D$condition

graph_D <- mutate(graph_D, traderel = trade/total )

graph_D <- mutate(graph_D, rel.freq.high.quality = product.quality*traderel, rel.freq.appeal = group.seller_appeal*traderel)

graph_D$rel.freq.appeal <- ifelse(graph_D$condition=="Voluntary", NA, graph_D$rel.freq.appeal)

graph_D$bid.low.rescale <- graph_D$bid.low/100

median.bid.low <- c(median(subset(Cohort.Main$bid.low, Cohort.Main$treatment == "Voluntary")), 
                    median(subset(Cohort.Sub$bid.low, Cohort.Sub$sub == "mu1")), 
                    median(subset(Cohort.Sub$bid.low, Cohort.Sub$sub == "mu2")))

graph_D <- mutate(graph_D, median.bid.low = median.bid.low[condition]/100)

graph_D <- mutate(graph_D, bid.theory = ifelse(sub=="0", 1.2, ifelse(sub=="mu1", 0.8, 0.64)))

# Manual legend

png(filename = "DataAnalysis/D.rel.freq.quality.png",height=800,width=800)  

p <- ggplot(graph_D, aes(x = bid.low.rescale)) +
  facet_wrap(condition~., nrow=3, labeller = labeller(condition = labs1))+
  geom_vline(aes(xintercept=median.bid.low), color="black", size=1)+
  geom_vline(aes(xintercept=bid.theory), color="black", linetype = "dashed", size=1)+
  geom_bar(aes(y=traderel), stat="identity", fill="grey", color="black", width=0.01)+
  geom_point(aes(y=rel.freq.high.quality, shape="Rel. freq. high quality weighted by number of trades"), size = 3) +
  scale_y_continuous(name = "Rel. freq. \n", limits = c(0,0.15)) + # ADJUST SCALE ACCORDINGLY
  scale_x_continuous(name = "\n Distribution of winning bids (normalized / 100)", limits = c(0.4, 1.4), breaks=seq(0.4, 1.4, 0.1), labels=seq(0.4, 1.4, 0.1)) + # ADJUST SCALE WITH NEW BIDS
  guides(shape=guide_legend(reverse = TRUE))+
  theme_bw() +
  theme(
    strip.text.x = element_text(size = 18),
    strip.text.y = element_text(size = 18),
    panel.border = element_blank(),
    panel.grid.major.y = element_line(color = "grey"),
    panel.grid.major.x = element_line(color = "grey"),
    text = element_text(color = "gray20", size = 16),
    axis.title.x = element_text(face="italic"),
    axis.title.y = element_text(face="italic"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.title=element_blank(),
    legend.text = element_text(size = 16),
    legend.spacing.x = unit(1.0, 'cm'),
    plot.caption = element_text(hjust=0, size = 12),
    plot.title = element_text(size = 16, face = "bold"))
p

dev.off()

##################################
### Non-parametric Tests of Hypotheses ###
##################################

  #' Statistical note from Oliver Kirchkamp on p-value correction: 
  #' If you have 10 independent hypotheses and you perform one test for each hypothesis, then, no, there is no reason to correct.
  #' If, instead, you have only one hypothesis, namely that one of the tasks shows an effect (and, ex ante, you don't know which of 
  #' the tasks) and you plan to perform 10 different tests and report only the one with the most impressive result, then, correct.

#' First check for order effects / learning on 3 decision variables using two-tailed Wilcoxon-Mann-Whitney test for unpaired two-sample comparison

  # Note currently this test lacks power to detect an effect

Cohort.arb.order <- aggregate(Arbitrator[c("bid", "strategy.quality", "ypct")],
                        by=list(Arbitrator$order, Arbitrator$cohort.number),
                        FUN=mean, na.rm=TRUE)

Cohort.arb.order <- rename(Cohort.arb.order, c("Group.1"="order", "Group.2"="cohort.number"))

wilcox.test(bid~order, data=Cohort.arb.order, alternative = c("two.sided"))
describeBy(Cohort.arb.order$bid, Cohort.arb.order$order)

wilcox.test(strategy.quality~order, data=Cohort.arb.order, alternative = c("two.sided"))
describeBy(Cohort.arb.order$strategy.quality, Cohort.arb.order$order)

wilcox.test(ypct~order, data=Cohort.arb.order, alternative = c("two.sided"))
describeBy(Cohort.arb.order$ypct, Cohort.arb.order$order)

#' Subset data for Main versus Sub comparisons

Cohort.Main.Sub <- aggregate(all.data[c("bid", "strategy.quality", "bid.low", "product.quality", "proposal.ratio", "proposal.ratio.low", "proposal.ratio.high", "ypct", "ypct.low", "ypct.high", "group.seller_appeal", "seller_appeal.low", "seller_appeal.high", "p", "p.low", "p.high", "group.buyer_payoff", "buyer_payoff.low", "buyer_payoff.high", "group.seller_payoff", "seller_payoff.low", "seller_payoff.high", "efficiency")],
                             by=list(all.data$sub, all.data$cohort.number),
                             FUN=mean, na.rm=TRUE)

Cohort.Main.Sub <- rename(Cohort.Main.Sub, c("Group.1"="sub", "Group.2"="cohort.number"))

Cohort.Main.mu1 <- subset(Cohort.Main.Sub, sub!="mu2")
Cohort.Main.mu1$sub <- droplevels(Cohort.Main.mu1$sub, exclude = if(anyNA(levels(Cohort.Main.mu1$sub))) NULL else NA)

Cohort.Main.mu2 <- subset(Cohort.Main.Sub, sub!="mu1")
Cohort.Main.mu2$sub <- droplevels(Cohort.Main.mu2$sub, exclude = if(anyNA(levels(Cohort.Main.mu2$sub))) NULL else NA)

#' Experienced sellers in new environment Arbitrator (check for learning)

experienced.data <- subset(Arbitrator, (period>10&period<16) | (period>25))

Cohort.Main.exp <- aggregate(experienced.data[c("bid", "strategy.quality", "bid.low", "product.quality", "proposal.ratio", "proposal.ratio.low", "proposal.ratio.high", "ypct", "ypct.low", "ypct.high", "group.seller_appeal", "seller_appeal.low", "seller_appeal.high", "p", "p.low", "p.high", "group.buyer_payoff", "buyer_payoff.low", "buyer_payoff.high", "group.seller_payoff", "seller_payoff.low", "seller_payoff.high", "efficiency")],
                         by=list(experienced.data$sub, experienced.data$cohort.number),
                         FUN=mean, na.rm=TRUE)

Cohort.Main.exp <- rename(Cohort.Main.exp, c("Group.1"="treatment", "Group.2"="cohort.number"))

#### Hypothesis: Winning bids are lower in Arbitrator than in Voluntary. 

  #' Point predictions: Two-tailed Wilcoxon Signed-Rank Test for one-sample comparison

#' Dictionary: one-sample Wilcoxon signed rank test is a non-parametric alternative to one-sample t-test when 
#' the data cannot be assumed to be normally distributed. It’s used to determine whether the 
#' ***median
#' of the sample is equal to a known standard value (i.e. theoretical value).
#' the data should be distributed symmetrically around the median. In other words, there should be roughly 
#' the same number of values above and below the median

boxplot(bid.low~treatment,data=Cohort.Main, main="Bids",
        xlab="Main Treatment", ylab="Winning bid")

Cohort.Sub$sub <- droplevels(Cohort.Sub$sub, exclude = if(anyNA(levels(Cohort.Sub$sub))) NULL else NA)

boxplot(bid.low~sub,data=Cohort.Sub, main="Bids",
        xlab="Sub-treatment", ylab="Winning bid")

df <- subset(Cohort.Main, treatment == "Voluntary")

wilcox.test(df$bid, mu = 120, alternative = "two.sided")

  #' Comparison: One-tailed Wilcoxon-Mann-Whitney test for unpaired two-sample comparison

wilcox.test(Cohort.Main$bid~Cohort.Main$treatment, alternative = c("greater")) 
tapply(Cohort.Main$bid, Cohort.Main$treatment, summary)

  wilcox.test(Cohort.Main.mu1$bid~Cohort.Main.mu1$sub, alternative = c("greater")) 

  wilcox.test(Cohort.Main.mu2$bid~Cohort.Main.mu2$sub, alternative = c("greater")) 

wilcox.test(Cohort.Main$bid.low~Cohort.Main$treatment, alternative = c("greater")) 
tapply(Cohort.Main$bid.low, Cohort.Main$treatment, summary)

  wilcox.test(Cohort.Main.mu1$bid.low~Cohort.Main.mu1$sub, alternative = c("greater")) 

  wilcox.test(Cohort.Main.mu2$bid.low~Cohort.Main.mu2$sub, alternative = c("greater")) 

  #' Bid profile / distribution
  #' WSR tests do not capture variety of bids observed

png(filename = "DataAnalysis/cdf.bids.png",height=800,width=800)  

p <- ggplot(all.data, aes(x = bid.rescale, linetype = condition)) +
  stat_ecdf()+
  scale_linetype_manual(name = "", values = c("solid","dotted","dotdash"), labels=labs1) +
  scale_y_continuous(name="", limits=c(0,1))+
  scale_x_continuous(name = "\n Bids (normalized / 100)", limits = c(0.3, 2), breaks=seq(0.3, 2, 0.1), labels=seq(0.3, 2, 0.1)) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(color = "gray20", size = 16),
    axis.title.x = element_text(face="italic"),
    axis.title.y = element_text(face="italic"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.title=element_blank(),
    legend.text = element_text(size = 16),
    legend.spacing.x = unit(1.0, 'cm'),
    plot.caption = element_text(hjust=0, size = 12),
    plot.title = element_text(size = 16, face = "bold"))
p

dev.off()

    # One-tailed Kolmogorov-Smirnov TestBids for two-sample comparison of bids using strategy method data

dist.Voluntary <- subset(all.data, treatment == "Voluntary", select=c(bid, bid.low))

dist.Arbitrator <- subset(all.data, treatment == "Arbitrator", select=c(bid, bid.low, sub))

ks.test(dist.Voluntary$bid, dist.Arbitrator$bid, alternative = c("less"))
# Here alternative = "less" means cdf of Voluntary is below [stochastically larger] and hence to the right of Arbitrator [reverse of wilcox.test]

summary(dist.Voluntary$bid)
summary(dist.Arbitrator$bid)

# And winning bids only

ks.test(dist.Voluntary$bid.low, dist.Arbitrator$bid.low, alternative = c("less"))

summary(dist.Voluntary$bid.low)
summary(dist.Arbitrator$bid.low)

#### Hypothesis: The relative frequency of high product quality is greater in Arbitrator than in Voluntary. 

  #' Comparison: One-tailed Wilcoxon-Mann-Whitney test for unpaired two-sample comparison

wilcox.test(Cohort.Main$strategy.quality~Cohort.Main$treatment, alternative = c("less")) 
tapply(Cohort.Main$strategy.quality, Cohort.Main$treatment, summary)

  wilcox.test(Cohort.Main.mu1$strategy.quality~Cohort.Main.mu1$sub, alternative = c("less")) 
  tapply(Cohort.Main.mu1$strategy.quality, Cohort.Main.mu1$sub, summary)
  
  wilcox.test(Cohort.Main.mu2$strategy.quality~Cohort.Main.mu2$sub, alternative = c("less")) 
  tapply(Cohort.Main.mu2$strategy.quality, Cohort.Main.mu2$sub, summary)

wilcox.test(Cohort.Main$product.quality~Cohort.Main$treatment, alternative = c("less")) 
tapply(Cohort.Main$product.quality, Cohort.Main$treatment, summary)

  wilcox.test(Cohort.Main.mu1$product.quality~Cohort.Main.mu1$sub, alternative = c("less")) 
  tapply(Cohort.Main.mu1$product.quality, Cohort.Main.mu1$sub, summary)
  
  wilcox.test(Cohort.Main.mu2$product.quality~Cohort.Main.mu2$sub, alternative = c("less")) 
  tapply(Cohort.Main.mu2$product.quality, Cohort.Main.mu2$sub, summary)

  #' And efficiency to account for arbitration cost

wilcox.test(Cohort.Main$efficiency~Cohort.Main$treatment, alternative = c("less")) 
tapply(Cohort.Main$efficiency, Cohort.Main$treatment, summary)

  wilcox.test(Cohort.Main.mu1$efficiency~Cohort.Main.mu1$sub, alternative = c("less")) 
  tapply(Cohort.Main.mu1$efficiency, Cohort.Main.mu1$sub, summary)
  
  wilcox.test(Cohort.Main.mu2$efficiency~Cohort.Main.mu2$sub, alternative = c("less")) 
  tapply(Cohort.Main.mu2$efficiency, Cohort.Main.mu2$sub, summary)

#### Hypothesis: In both Voluntary and Arbitrator, the buyer proposes the lower bound contract price; 
#' the final trading price is greater in Arbitrator than in Voluntary.

  # One-sided test since mechanically cannot be < 0.25

  # Point prediction

df <- subset(Cohort.Main, treatment == "Voluntary")

summary(df$proposal.ratio)

wilcox.test(df$proposal.ratio, mu = 0.25, alternative = "greater")
wilcox.test(df$proposal.ratio.low, mu = 0.25, alternative = "greater")
wilcox.test(df$proposal.ratio.high, mu = 0.25, alternative = "greater")

df <- subset(Cohort.Main, treatment == "Arbitrator")

summary(df$proposal.ratio)

wilcox.test(df$proposal.ratio, mu = 0.25, alternative = "greater")
wilcox.test(df$proposal.ratio.low, mu = 0.25, alternative = "greater")
wilcox.test(df$proposal.ratio.high, mu = 0.25, alternative = "greater")

  # Reciprocity (conditional on price)

# Lower in Arbitrator = substitute / crowding out reciprocity

wilcox.test(Cohort.Main$ypct~Cohort.Main$treatment, alternative = c("two.sided"))
tapply(Cohort.Main$ypct, Cohort.Main$treatment, summary)

wilcox.test(Cohort.Main$ypct.low~Cohort.Main$treatment, alternative = c("two.sided")) 
tapply(Cohort.Main$ypct.low, Cohort.Main$treatment, summary)
# low quality really punished

wilcox.test(Cohort.Main$ypct.high~Cohort.Main$treatment, alternative = c("two.sided")) 
tapply(Cohort.Main$ypct.high, Cohort.Main$treatment, summary)
# high quality less so 

# Receipt of low versus high quality

wilcox.test(subset(Cohort.Main$ypct.low, Cohort.Main$treatment == "Voluntary"), subset(Cohort.Main$ypct.high, Cohort.Main$treatment == "Voluntary"), alternative = c("two.sided"), paired = TRUE) 
summary(subset(Cohort.Main$ypct.low, Cohort.Main$treatment == "Voluntary"))
summary(subset(Cohort.Main$ypct.high, Cohort.Main$treatment == "Voluntary"))

wilcox.test(subset(Cohort.Main$ypct.low, Cohort.Main$treatment == "Arbitrator"), subset(Cohort.Main$ypct.high, Cohort.Main$treatment == "Arbitrator"), alternative = c("two.sided"), paired = TRUE) 
summary(subset(Cohort.Main$ypct.low, Cohort.Main$treatment == "Arbitrator"))
summary(subset(Cohort.Main$ypct.high, Cohort.Main$treatment == "Arbitrator"))

  # Final trading prices

df <- subset(Cohort.Main, treatment == "Voluntary")

wilcox.test(df$p, mu = 30, alternative = "two.sided")

df <- subset(Cohort.Main, treatment == "Arbitrator")

wilcox.test(df$p, mu = 40, alternative = "two.sided")

wilcox.test(Cohort.Main$p~Cohort.Main$treatment, alternative = c("less")) 
tapply(Cohort.Main$p, Cohort.Main$treatment, summary)

#### Hypothesis: Winning bids are higher when μ=1/3 than when μ=2/3; product quality and price proposals remain unchanged.

  # Bids

#' Comparison: One-tailed Wilcoxon Signed Rank test for paired sample comparison

wilcox.test(Cohort.Sub$bid~Cohort.Sub$sub, alternative = c("two.sided"), paired = TRUE) 
tapply(Cohort.Sub$bid, Cohort.Sub$sub, summary)

wilcox.test(Cohort.Sub$bid.low~Cohort.Sub$sub, alternative = c("two.sided"), paired = TRUE) 
tapply(Cohort.Sub$bid.low, Cohort.Sub$sub, summary)

# One-tailed Kolmogorov-Smirnov TestBids for two-sample comparison of bids using strategy method data

dist.mu1 <- subset(dist.Arbitrator, sub == "mu1")
dist.mu2 <- subset(dist.Arbitrator, sub == "mu2")

ks.test(dist.mu1$bid, dist.mu2$bid, alternative = c("two.sided"))

summary(dist.mu1$bid)
summary(dist.mu2$bid)

# And winning bids only

ks.test(dist.mu1$bid.low, dist.mu2$bid.low, alternative = c("two.sided"))

summary(dist.mu1$bid.low)
summary(dist.mu2$bid.low)

# Check spillovers. Check mu1 for order 12 and mu2 for order 21 against predictions
# corresponds to periods 1 to 15

no.spillover.sub <- subset(Cohort.Sub, (sub == "mu1" & order == "12")|(sub == "mu2" & order == "21"))
no.spillover.sub$sub <- droplevels(no.spillover.sub$sub, exclude = if(anyNA(levels(no.spillover.sub$sub))) NULL else NA)

des.no.spillover.sub <- describeBy(no.spillover.sub, no.spillover.sub$sub, mat=TRUE,digits=2)  #matrix output

des.no.spillover.sub <- rename(des.no.spillover.sub, c("group1"="Sub"))
des.no.spillover.sub <- subset(des.no.spillover.sub, select=c(2, 7))
des.no.spillover.sub <- des.no.spillover.sub[-c(1:6, 53:54), ]

write_csv(des.no.spillover.sub, "DataAnalysis/SummStatNoSpilloverArb.csv")

# Check learning

Cohort.Main.exp$treatment <- droplevels(Cohort.Main.exp$treatment, exclude = if(anyNA(levels(Cohort.Main.exp$treatment))) NULL else NA)
des.experienced.sub <- describeBy(Cohort.Main.exp, Cohort.Main.exp$treatment,
                                  mat=TRUE,digits=2)  #matrix output

des.experienced.sub <- rename(des.experienced.sub, c("group1"="Sub"))
des.experienced.sub <- subset(des.experienced.sub, select=c(2, 7))
des.experienced.sub <- des.experienced.sub[-c(1:6, 53:54), ]

write_csv(des.experienced.sub, "DataAnalysis/SummStatExperiencedArb.csv")

wilcox.test(Cohort.Main.exp$bid~Cohort.Main.exp$treatment, alternative = c("two.sided"), paired = TRUE) 
tapply(Cohort.Main.exp$bid, Cohort.Main.exp$treatment, summary)

wilcox.test(Cohort.Main.exp$bid.low~Cohort.Main.exp$treatment, alternative = c("two.sided"), paired = TRUE) 
tapply(Cohort.Main.exp$bid.low, Cohort.Main.exp$treatment, summary)

  # Quality

wilcox.test(Cohort.Sub$product.quality~Cohort.Sub$sub, alternative = c("two.sided"), paired = TRUE) 
tapply(Cohort.Sub$product.quality, Cohort.Sub$sub, summary)

wilcox.test(Cohort.Sub$efficiency~Cohort.Sub$sub, alternative = c("two.sided"), paired = TRUE) 
tapply(Cohort.Sub$efficiency, Cohort.Sub$sub, summary)

  # Proposals

df <- subset(Cohort.Sub, sub == "mu1")

wilcox.test(df$proposal.ratio, mu = 0.25, alternative = "greater")
wilcox.test(df$proposal.ratio.low, mu = 0.25, alternative = "greater")
wilcox.test(df$proposal.ratio.high, mu = 0.25, alternative = "greater")

df <- subset(Cohort.Sub, sub == "mu2")

wilcox.test(df$proposal.ratio, mu = 0.25, alternative = "greater")
wilcox.test(df$proposal.ratio.low, mu = 0.25, alternative = "greater")
wilcox.test(df$proposal.ratio.high, mu = 0.25, alternative = "greater")

    # Reciprocity (conditional on price)

wilcox.test(Cohort.Sub$ypct~Cohort.Sub$sub, alternative = c("less"), paired = TRUE) 
tapply(Cohort.Sub$ypct, Cohort.Sub$sub, summary)

wilcox.test(Cohort.Sub$ypct.low~Cohort.Sub$sub, alternative = c("less"), paired = TRUE) 
tapply(Cohort.Sub$ypct.low, Cohort.Sub$sub, summary)

wilcox.test(Cohort.Sub$ypct.high~Cohort.Sub$sub, alternative = c("less"), paired = TRUE) 
tapply(Cohort.Sub$ypct.high, Cohort.Sub$sub, summary)

  # Rel. freq arbitration
  
wilcox.test(Cohort.Sub$group.seller_appeal~Cohort.Sub$sub, alternative = c("two.sided"), paired = TRUE) 
tapply(Cohort.Sub$group.seller_appeal, Cohort.Sub$sub, summary)

wilcox.test(Cohort.Sub$seller_appeal.low~Cohort.Sub$sub, alternative = c("two.sided"), paired = TRUE) 
tapply(Cohort.Sub$seller_appeal.low, Cohort.Sub$sub, summary)

wilcox.test(Cohort.Sub$seller_appeal.high~Cohort.Sub$sub, alternative = c("two.sided"), paired = TRUE) 
tapply(Cohort.Sub$seller_appeal.high, Cohort.Sub$sub, summary)

  # Final trading prices

df <- subset(Cohort.Sub, sub == "mu1")

wilcox.test(df$p, mu = 40, alternative = "two.sided")
wilcox.test(df$p.high, mu = 40, alternative = "two.sided")

df <- subset(Cohort.Sub, sub == "mu2")

wilcox.test(df$p, mu = 40, alternative = "two.sided")
wilcox.test(df$p.high, mu = 40, alternative = "two.sided")

wilcox.test(Cohort.Sub$p~Cohort.Sub$sub, alternative = c("less"), paired = TRUE) 
tapply(Cohort.Sub$p, Cohort.Sub$sub, summary)

wilcox.test(Cohort.Sub$p.high~Cohort.Sub$sub, alternative = c("less"), paired = TRUE) 
tapply(Cohort.Sub$p.high, Cohort.Sub$sub, summary)

wilcox.test(Cohort.Sub$p.low~Cohort.Sub$sub, alternative = c("less"), paired = TRUE) 
tapply(Cohort.Sub$p.low, Cohort.Sub$sub, summary)

#### Hypothesis: Buyer profits are higher and seller profits are no lower in Arbitrator than in Voluntary.

# Comparative by role within main treatments

df <- subset(Cohort.Main, treatment == "Voluntary")

wilcox.test(df$group.buyer_payoff, df$group.seller_payoff, alternative = c("greater"), paired=TRUE) 
summary(df$group.buyer_payoff)
summary(df$group.seller_payoff)

df <- subset(Cohort.Main, treatment == "Arbitrator")

wilcox.test(df$group.buyer_payoff, df$group.seller_payoff, alternative = c("greater"), paired=TRUE) 
summary(df$group.buyer_payoff)
summary(df$group.seller_payoff)

# Point prediction

df <- subset(Cohort.Main, treatment == "Voluntary")

wilcox.test(df$group.buyer_payoff, mu = 19, alternative = "two.sided")
wilcox.test(df$group.seller_payoff, mu = 1, alternative = "two.sided")

df <- subset(Cohort.Main, treatment == "Arbitrator")

wilcox.test(df$group.buyer_payoff, mu = 59, alternative = "two.sided")
wilcox.test(df$group.seller_payoff, mu = 1, alternative = "two.sided")

# Comparative by quality within treatment / role

df <- subset(Cohort.Main, treatment == "Voluntary")

wilcox.test(df$buyer_payoff.low, df$buyer_payoff.high, alternative = c("two.sided"), paired=TRUE) 
summary(df$buyer_payoff.low)
summary(df$buyer_payoff.high)

df <- subset(Cohort.Main, treatment == "Arbitrator")

wilcox.test(df$buyer_payoff.low, df$buyer_payoff.high, alternative = c("two.sided"), paired=TRUE) 
summary(df$buyer_payoff.low)
summary(df$buyer_payoff.high)

df <- subset(Cohort.Sub, sub == "mu1")

wilcox.test(df$buyer_payoff.low, df$buyer_payoff.high, alternative = c("two.sided"), paired=TRUE) 
summary(df$buyer_payoff.low)
summary(df$buyer_payoff.high)

df <- subset(Cohort.Sub, sub == "mu2")

wilcox.test(df$buyer_payoff.low, df$buyer_payoff.high, alternative = c("two.sided"), paired=TRUE) 
summary(df$buyer_payoff.low)
summary(df$buyer_payoff.high)

# Comparative between main versus sub-treatments / within sub-treatments
# In direction theorised

  # Buyer

wilcox.test(Cohort.Main$group.buyer_payoff~Cohort.Main$treatment, alternative = c("less")) 
tapply(Cohort.Main$group.buyer_payoff, Cohort.Main$treatment, summary)

wilcox.test(Cohort.Main.mu1$group.buyer_payoff~Cohort.Main.mu1$sub, alternative = c("less")) 
tapply(Cohort.Main.mu1$group.buyer_payoff, Cohort.Main.mu1$sub, summary)

wilcox.test(Cohort.Main.mu2$group.buyer_payoff~Cohort.Main.mu2$sub, alternative = c("less")) 
tapply(Cohort.Main.mu2$group.buyer_payoff, Cohort.Main.mu2$sub, summary)

wilcox.test(Cohort.Sub$group.buyer_payoff~Cohort.Sub$sub, alternative = c("two.sided"), paired=TRUE) 
tapply(Cohort.Sub$group.buyer_payoff, Cohort.Sub$sub, summary)

  # Seller

wilcox.test(Cohort.Main$group.seller_payoff~Cohort.Main$treatment, alternative = c("two.sided")) 
tapply(Cohort.Main$group.seller_payoff, Cohort.Main$treatment, summary)

wilcox.test(Cohort.Main.mu1$group.seller_payoff~Cohort.Main.mu1$sub, alternative = c("two.sided")) 
tapply(Cohort.Main.mu1$group.seller_payoff, Cohort.Main.mu1$sub, summary)

wilcox.test(Cohort.Main.mu2$group.seller_payoff~Cohort.Main.mu2$sub, alternative = c("two.sided")) 
tapply(Cohort.Main.mu2$group.seller_payoff, Cohort.Main.mu2$sub, summary)

wilcox.test(Cohort.Sub$group.seller_payoff~Cohort.Sub$sub, alternative = c("two.sided"), paired=TRUE) 
tapply(Cohort.Sub$group.seller_payoff, Cohort.Sub$sub, summary)

# Comparative by quality between main versus sub-treatments / within sub-treatments

  # Buyers

    # Low quality

wilcox.test(Cohort.Main.mu1$buyer_payoff.low~Cohort.Main.mu1$sub, alternative = c("less")) 
tapply(Cohort.Main.mu1$buyer_payoff.low, Cohort.Main.mu1$sub, summary)

wilcox.test(Cohort.Main.mu2$buyer_payoff.low~Cohort.Main.mu2$sub, alternative = c("less")) 
tapply(Cohort.Main.mu2$buyer_payoff.low, Cohort.Main.mu2$sub, summary)

wilcox.test(Cohort.Sub$buyer_payoff.low~Cohort.Sub$sub, alternative = c("two.sided"), paired=TRUE) 
tapply(Cohort.Sub$buyer_payoff.low, Cohort.Sub$sub, summary)

    # High quality

wilcox.test(Cohort.Main.mu1$buyer_payoff.high~Cohort.Main.mu1$sub, alternative = c("less")) 
tapply(Cohort.Main.mu1$buyer_payoff.high, Cohort.Main.mu1$sub, summary)

wilcox.test(Cohort.Main.mu2$buyer_payoff.high~Cohort.Main.mu2$sub, alternative = c("less")) 
tapply(Cohort.Main.mu2$buyer_payoff.high, Cohort.Main.mu2$sub, summary)

wilcox.test(Cohort.Sub$buyer_payoff.high~Cohort.Sub$sub, alternative = c("two.sided"), paired=TRUE) 
tapply(Cohort.Sub$buyer_payoff.high, Cohort.Sub$sub, summary)

  # Sellers

    # Low quality

wilcox.test(Cohort.Main.mu1$seller_payoff.low~Cohort.Main.mu1$sub, alternative = c("two.sided")) 
tapply(Cohort.Main.mu1$seller_payoff.low, Cohort.Main.mu1$sub, summary)

wilcox.test(Cohort.Main.mu2$seller_payoff.low~Cohort.Main.mu2$sub, alternative = c("two.sided")) 
tapply(Cohort.Main.mu2$seller_payoff.low, Cohort.Main.mu2$sub, summary)

wilcox.test(Cohort.Sub$seller_payoff.low~Cohort.Sub$sub, alternative = c("less"), paired=TRUE) 
tapply(Cohort.Sub$seller_payoff.low, Cohort.Sub$sub, summary)

    # High quality

wilcox.test(Cohort.Main.mu1$seller_payoff.high~Cohort.Main.mu1$sub, alternative = c("two.sided")) 
tapply(Cohort.Main.mu1$seller_payoff.high, Cohort.Main.mu1$sub, summary)

wilcox.test(Cohort.Main.mu2$seller_payoff.high~Cohort.Main.mu2$sub, alternative = c("two.sided")) 
tapply(Cohort.Main.mu2$seller_payoff.high, Cohort.Main.mu2$sub, summary)

wilcox.test(Cohort.Sub$seller_payoff.high~Cohort.Sub$sub, alternative = c("two.sided"), paired=TRUE) 
tapply(Cohort.Sub$seller_payoff.high, Cohort.Sub$sub, summary)

##################################
### Power check for null results Mu2 versus 0 ###
##################################

mean1 <- mean(subset(Cohort.Main.mu2$efficiency, Cohort.Main.mu2$sub == "0"))
sd1 <- sd(subset(Cohort.Main.mu2$efficiency, Cohort.Main.mu2$sub == "0"))
mean2 <- mean(subset(Cohort.Main.mu2$efficiency, Cohort.Main.mu2$sub == "mu2"))
sd2 <- sd(subset(Cohort.Main.mu2$efficiency, Cohort.Main.mu2$sub == "mu2"))

mean1 <- mean(subset(Cohort.Main.mu2$group.seller_payoff, Cohort.Main.mu2$sub == "0"))
sd1 <- sd(subset(Cohort.Main.mu2$group.seller_payoff, Cohort.Main.mu2$sub == "0"))
mean2 <- mean(subset(Cohort.Main.mu2$group.seller_payoff, Cohort.Main.mu2$sub == "mu2"))
sd2 <- sd(subset(Cohort.Main.mu2$group.seller_payoff, Cohort.Main.mu2$sub == "mu2"))

  # Check power

N <- 6

pval <- replicate(1000, wilcox.test(rnorm(N,mean1,sd1), rnorm(N,mean2,sd2), alternative = c("less"))$p.value)
summary(pval)
# Power (%)
(sum(pval < .05) / 1000) * 100

# # Sample takes a sample of the specified size from the elements of x using either with or without replacement
# 
# p.values <- length (1000)     # Empty object to collect p-values
# 
# for (i in 1 : 1000) {
#   
#   vol.resample <- sample(subset(Cohort.Main.mu2$group.seller_payoff, Cohort.Main.mu2$sub == "0"), N, replace = TRUE)
#   arb.resample <- sample(subset(Cohort.Main.mu2$group.seller_payoff, Cohort.Main.mu2$sub == "mu2"), N, replace = TRUE)
#   p.values[i] <- wilcox.test(vol.resample, arb.resample, alternative = c("less"))$p.value
#   
# }

# summary(p.values)

##################################
### Arbitrator Randomization check ###
##################################

arb.available <- na.omit(Arbitrator$arb.available)

#' Two-tailed binomial test of group-level data 
binom.test(sum(arb.available), length(arb.available), p = 0.5,
           alternative = c("t"),
           conf.level = 0.95)

##################################
### Analysis of matching group data ###
##################################

# prep data

matching.data <- aggregate(all.data[c("bid.low", "product.quality","efficiency","ypct","group.buyer_payoff","group.seller_payoff")],
                        by=list(all.data$sub, all.data$cohort.number, all.data$period, all.data$group.period.id),
                        FUN=mean, na.rm=TRUE)

matching.data <- rename(matching.data, c("Group.1"="sub", "Group.2"="cohort.number", "Group.3"="period", "Group.4"="group.period.id"))

### STATA export for matching group data analysis ###

library(foreign)
write.dta(matching.data, "DataAnalysis/Stata/Data/matching_gp_data.dta")

## R Version ###

# ols
ols1 <- lm(data = matching.data, formula=bid.low ~ sub + period) 
summary(ols1)

# null.model <- lm(data = matching.data, formula=bid.low ~ 1)
# summary(null.model)
# anova(null.model, ols1, test="Chisq")

# check heteroskedasticity
bptest(ols1)

# cluster-robust SEs
validation.1 <- miceadds::lm.cluster( data = matching.data, formula=bid.low ~ sub + period, cluster="cohort.number" )
summary(validation.1)

validation.2 <- miceadds::glm.cluster( data = matching.data, formula=product.quality ~ sub + period, cluster="cohort.number", family="binomial" )
summary(validation.2)

linearHypothesis(validation.2, "submu1 = submu2")

# Find residual/null deviances/dfs from glm cluster model
# The difference between the null deviance and the model's deviance is distributed as a chi-squared 
# with degrees of freedom equal to the null df minus the model's df
# By default, pchisq() gives the proportion of the distribution to the left of the value. 
# To get the proportion more extreme than your difference, you can specify lower.tail = FALSE 
# or subtract the result from 1
pchisq(1478.8 - 1469.7, 1069 - 1066, lower.tail = FALSE)

# Alternatively
logit2 <- glm(data = matching.data, formula=product.quality ~ sub + period, family="binomial") 
logit2
null.model <- glm(data = matching.data, formula=product.quality ~ 1, family="binomial")
summary(null.model)
anova.2 <- anova(null.model, logit2, test="Chisq")
anova.2

validation.3 <- miceadds::lm.cluster( data = matching.data, formula=ypct ~ sub + period, cluster="cohort.number" )
summary(validation.3)

linearHypothesis(validation.3, "submu1 = submu2")

ols.3 <- lm( data = matching.data, formula=ypct ~ sub + period)
summary(ols.3)

validation.4 <- miceadds::lm.cluster( data = matching.data, formula=group.buyer_payoff ~ sub + period, cluster="cohort.number" )
summary(validation.4)

linearHypothesis(validation.4, "submu1 = submu2")

ols.4 <- lm( data = matching.data, formula=group.buyer_payoff ~ sub + period)
summary(ols.4)

validation.5 <- miceadds::lm.cluster( data = matching.data, formula=group.seller_payoff ~ sub + period, cluster="cohort.number" )
summary(validation.5)

linearHypothesis(validation.5, "submu1 = submu2")

ols.5 <- lm( data = matching.data, formula=group.seller_payoff ~ sub + period)
summary(ols.5)

##################################
### Seller bidding strategies ###
##################################

### Regression Analysis ###

  # Stata Export [for multi-level modelling ] #

reg.vol.seller<- subset(Voluntary, role == "Seller")
reg.mu1.seller<- subset(Arbitrator, role == "Seller" & sub == "mu1")
reg.mu2.seller<- subset(Arbitrator, role == "Seller" & sub == "mu2")

reg.vol.seller$risk.1 <- as.numeric(reg.vol.seller$player.risk1)
reg.vol.seller$risk.2 <- as.numeric(reg.vol.seller$player.risk2)
reg.vol.seller$trust.1 <- as.numeric(reg.vol.seller$player.trust.index)
reg.vol.seller$trust.2 <- as.factor(as.numeric(reg.vol.seller$player.trust_strangers) - 1)
reg.vol.seller$female <- as.factor(ifelse(reg.vol.seller$player.gender == "Female", 1, 0))
reg.vol.seller$econ <- as.factor(reg.vol.seller$econ)

reg.vol.seller <- reg.vol.seller[order(reg.vol.seller$subject.id, reg.vol.seller$period),]

reg.vol.seller <- 
  reg.vol.seller %>%
  group_by(subject.id) %>%
  mutate(lag.bid = dplyr::lag(bid, n = 1, default = NA)) %>%
  mutate(lag.bid = ifelse(period == 1, NA, lag.bid)) %>%
  mutate(lag.winner = dplyr::lag(winner, n = 1, default = NA)) %>%
  mutate(lag.winner = ifelse(period == 1, NA, lag.winner)) %>%
  mutate(lag.bid.low = dplyr::lag(bid.low, n = 1, default = NA)) %>%
  mutate(lag.bid.low = ifelse(period == 1, NA, lag.bid.low)) %>%
  mutate(lag.bid.high = dplyr::lag(bid.high, n = 1, default = NA)) %>%
  mutate(lag.bid.high = ifelse(period == 1, NA, lag.bid.high)) %>%
  mutate(lag.comp.bid = ifelse(lag.winner, lag.bid.high, lag.bid.low))

reg.mu1.seller$risk.1 <- as.numeric(reg.mu1.seller$player.risk1)
reg.mu1.seller$risk.2 <- as.numeric(reg.mu1.seller$player.risk2)
reg.mu1.seller$trust.1 <- as.numeric(reg.mu1.seller$player.trust.index)
reg.mu1.seller$trust.2 <- as.factor(as.numeric(reg.mu1.seller$player.trust_strangers) - 1)
reg.mu1.seller$female <- as.factor(ifelse(reg.mu1.seller$player.gender == "Female", 1, 0))
reg.mu1.seller$econ <- as.factor(reg.mu1.seller$econ)

reg.mu1.seller <- reg.mu1.seller[order(reg.mu1.seller$subject.id, reg.mu1.seller$period),]

reg.mu1.seller <- 
  reg.mu1.seller %>%
  group_by(subject.id) %>%
  mutate(lag.bid = dplyr::lag(bid, n = 1, default = NA)) %>%
  mutate(lag.bid = ifelse(period == 1, NA, lag.bid)) %>%
  mutate(lag.winner = dplyr::lag(winner, n = 1, default = NA)) %>%
  mutate(lag.winner = ifelse(period == 1, NA, lag.winner)) %>%
  mutate(lag.bid.low = dplyr::lag(bid.low, n = 1, default = NA)) %>%
  mutate(lag.bid.low = ifelse(period == 1, NA, lag.bid.low)) %>%
  mutate(lag.bid.high = dplyr::lag(bid.high, n = 1, default = NA)) %>%
  mutate(lag.bid.high = ifelse(period == 1, NA, lag.bid.high)) %>%
  mutate(lag.comp.bid = ifelse(lag.winner, lag.bid.high, lag.bid.low))

reg.mu2.seller$risk.1 <- as.numeric(reg.mu2.seller$player.risk1)
reg.mu2.seller$risk.2 <- as.numeric(reg.mu2.seller$player.risk2)
reg.mu2.seller$trust.1 <- as.numeric(reg.mu2.seller$player.trust.index)
reg.mu2.seller$trust.2 <- as.factor(as.numeric(reg.mu2.seller$player.trust_strangers) - 1)
reg.mu2.seller$female <- as.factor(ifelse(reg.mu2.seller$player.gender == "Female", 1, 0))
reg.mu2.seller$econ <- as.factor(reg.mu2.seller$econ)

reg.mu2.seller <- reg.mu2.seller[order(reg.mu2.seller$subject.id, reg.mu2.seller$period),]

reg.mu2.seller <- 
  reg.mu2.seller %>%
  group_by(subject.id) %>%
  mutate(lag.bid = dplyr::lag(bid, n = 1, default = NA)) %>%
  mutate(lag.bid = ifelse(period == 1, NA, lag.bid)) %>%
  mutate(lag.winner = dplyr::lag(winner, n = 1, default = NA)) %>%
  mutate(lag.winner = ifelse(period == 1, NA, lag.winner)) %>%
  mutate(lag.bid.low = dplyr::lag(bid.low, n = 1, default = NA)) %>%
  mutate(lag.bid.low = ifelse(period == 1, NA, lag.bid.low)) %>%
  mutate(lag.bid.high = dplyr::lag(bid.high, n = 1, default = NA)) %>%
  mutate(lag.bid.high = ifelse(period == 1, NA, lag.bid.high)) %>%
  mutate(lag.comp.bid = ifelse(lag.winner, lag.bid.high, lag.bid.low))

reg.stata.seller <- rbind(reg.vol.seller, reg.mu1.seller, reg.mu2.seller)

library(foreign)
write.dta(reg.stata.seller, "DataAnalysis/Stata/Data/seller_reg_data.dta")

### Strategy classification ###

  # Theory (Risk Neutral case)

sigma <- c(0,0.5)                # arbitration probability
mu <- c(1/3, 2/3)       # arbitrator preference
lambda <- 0.75              # contract flexibility
N <- c(2)
vL <- 0.50  
vH <- 1                     # Trust Game multiplier 3 x
cL <- 0.30
cH <- 0.40
Delta <- 0.01               # bid increment
b_max <- 2                  # max bid
k <- 2

sim.data <- data.frame()

for (k in sigma) {
for (i in mu) {

      df.sim.data <- expand.grid(b=seq(cL, b_max, Delta), mu=i, sigma = k)
      df.sim.data <- mutate(df.sim.data, p_lower = (1-lambda)*b, p_upper = b) 
      df.sim.data <- mutate(df.sim.data, zH = pmin(p_upper,mu*vH + (1-mu)*cH), zL= pmin(p_upper, mu*vL + (1-mu)*cL))
      df.sim.data <- mutate(df.sim.data, pA.H = pmax(zH,p_lower), pA.L = pmax(zL,p_lower))    # Assuming dispute
      df.sim.data <- mutate(df.sim.data, dSH = ((1-sigma)*p_lower+sigma*pA.H-cH)/N,                                           
                   dSL = ((1-sigma)*p_lower+sigma*pA.L-cL)/N)
      df.sim.data <- mutate(df.sim.data, dSMax = pmax(dSH,dSL))
      lookup <- df.sim.data %>%
        select(b, dSMax)
      
      lookup$b <- lookup$b + Delta
      lookup <- subset(lookup, b <= b_max)
      lookup <- rename(lookup, c("dSMax"="dSMaxMinus"))
      lookup$b <- as.character(lookup$b)                                                                      # necessary for merge due to floating point artifacts
      lookup$b <- as.numeric(lookup$b)
      
      df.sim.data$b <- as.character(df.sim.data$b)
      df.sim.data$b <- as.numeric(df.sim.data$b)
      df.sim.data <- merge(df.sim.data, lookup, all.x = TRUE)
      
      df.sim.data <- mutate(df.sim.data, dev = ifelse(b>cL, (dSMaxMinus*N)-dSMax, NA), 
                   quality = ifelse(dSMax==dSH, 1,0),                                                         # incentive compatible quality
                   dSH.a1 = dSH*N, dSL.a1 = dSL*N                                                             # expected seller dispute payoff conditional on acceptance
      )
      
      sim.data <- rbind(sim.data, df.sim.data)
      
}
  
}

sim.to.main <- sim.data[,c(1,2,3,12,13,14,15,16,17)]
sim.to.main <- rename(sim.to.main, c("b"="bid.rescale", "mu"="par.mu", "sigma"="par.sigma", "quality"="theory.q"))
sim.to.main <- subset(sim.to.main, par.sigma == 0.5 | (par.sigma == 0 & par.mu < 0.5))
sim.to.main <- mutate(sim.to.main, par.mu = ifelse(par.sigma == 0, NA, par.mu))

seller.strategy.vol <- subset(Voluntary, role == "Seller")
seller.strategy.vol$bid.rescale <- seller.strategy.vol$bid/100
seller.strategy.vol <- merge(seller.strategy.vol, sim.to.main, by = c("par.sigma","par.mu","bid.rescale"), all.x=T, sort=F)
seller.strategy.vol <- mutate(seller.strategy.vol, dS = ifelse(strategy.quality == 0, dSL.a1, dSH.a1))
seller.strategy.vol <- mutate(seller.strategy.vol, IC = ifelse(strategy.quality == theory.q, 1, ifelse(is.na(strategy.quality), NA, 0)), IR = ifelse(dS>=0, 1, ifelse(is.na(dS), NA, 0)), count = ifelse(!is.na(bid), 1, NA))

summary(seller.strategy.vol$IC)
summary(seller.strategy.vol$IR)

seller.strategy.arb <- subset(Arbitrator, role == "Seller")
seller.strategy.arb$bid.rescale <- seller.strategy.arb$bid/100
seller.strategy.arb <- merge(seller.strategy.arb, sim.to.main, by = c("par.sigma","par.mu","bid.rescale"), all.x=T, sort=F)
seller.strategy.arb <- mutate(seller.strategy.arb, dS = ifelse(strategy.quality == 0, dSL.a1, dSH.a1))
seller.strategy.arb <- mutate(seller.strategy.arb, IC = ifelse(strategy.quality == theory.q, 1, ifelse(is.na(strategy.quality), NA, 0)), IR = ifelse(dS>=0, 1, ifelse(is.na(dS), NA, 0)), count = ifelse(!is.na(bid), 1, NA))

summary(seller.strategy.arb$IC)
summary(seller.strategy.arb$IR)
tapply(seller.strategy.arb$IC, seller.strategy.arb$sub, summary)
tapply(seller.strategy.arb$IR, seller.strategy.arb$sub, summary)

# Number of rational and incentive compatible strategies per individual seller.
# Observation per seller - mu - based on 36 sellers

agg.seller.strategy.arb <- aggregate(seller.strategy.arb[c("IC", "IR", "count")], by=list(seller.strategy.arb$sub, seller.strategy.arb$subject.id),
                                           FUN=sum, na.rm=TRUE)

agg.seller.strategy.arb <- rename(agg.seller.strategy.arb, c("Group.1"="sub", "Group.2"="subject.id"))

agg.seller.strategy.arb <- mutate(agg.seller.strategy.arb, prop.IC = IC/count, prop.IR = IR/count)

agg.seller.strategy.arb$sub <- factor(agg.seller.strategy.arb$sub)

tapply(agg.seller.strategy.arb$prop.IC, agg.seller.strategy.arb$sub, summary)
tapply(agg.seller.strategy.arb$prop.IR, agg.seller.strategy.arb$sub, summary)

dupe.vector <- duplicated(agg.seller.strategy.arb[,c(1,3,4)]) 

agg.seller.strategy.arb$dupe <- dupe.vector
# check no. duplicate rows within mu for below scaling based on actual number IR/IC choices out of (max) 15
# sort by IR - IC - mu then for each consecutive TRUE have 1 dupe

png(filename = "DataAnalysis/seller.IC.IR.png",height=800,width=800)  

p <- ggplot(
  agg.seller.strategy.arb, 
  aes(x = IR, y=IC)
) +
  geom_count(show.legend = FALSE, shape=19) +
  scale_size(range = c(3, 9)) + # adjust based on no. sellers with same coordinate [current max = 3x]
  facet_wrap(sub~., nrow=2, labeller = labeller(sub = labs1))+
  scale_x_continuous(limits=c(0,15))+
  scale_y_continuous(limits=c(0,15))+
  labs(x = "\n Number of individually rational auction strategies", y = "Number of incentive compatible auction strategies \n")+
  theme_bw() +
  theme(
    strip.text.x = element_text(size = 18),
    strip.text.y = element_text(size = 18),
    panel.border = element_blank(),
    panel.grid.major.y = element_line(color = "grey"),
    panel.grid.major.x = element_line(color = "grey"),
    text = element_text(color = "gray20", size = 16),
    axis.title.x = element_text(face="italic"),
    axis.title.y = element_text(face="italic"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.title=element_blank(),
    legend.text = element_text(size = 16),
    legend.spacing.x = unit(1.0, 'cm'),
    plot.caption = element_text(hjust=0, size = 12),
    plot.title = element_text(size = 16, face = "bold"))
p

dev.off()

# Rationality and compatibility of seller cohorts over time. 
# Observation per cohort - period - mu - based on 6 sellers per cohort-period

  # Voluntary

seller.strategy.vol <- mutate(seller.strategy.vol, time = period)

cohort.seller.strategy.vol <- aggregate(seller.strategy.vol[c("IC", "IR", "count")], by=list(seller.strategy.vol$cohort.number, seller.strategy.vol$time),
                                        FUN=sum, na.rm=TRUE)

cohort.seller.strategy.vol <- rename(cohort.seller.strategy.vol, c("Group.1"="cohort.number", "Group.2"="time"))

  # Arbitrator

seller.strategy.arb <- mutate(seller.strategy.arb, 
                              time = ifelse((order=="12"&sub=="mu1")|(order=="21"&sub=="mu2"), period, period-15))

cohort.seller.strategy.arb <- aggregate(seller.strategy.arb[c("IC", "IR", "count")], by=list(seller.strategy.arb$sub, seller.strategy.arb$cohort.number, seller.strategy.arb$time),
                                     FUN=sum, na.rm=TRUE)

cohort.seller.strategy.arb <- rename(cohort.seller.strategy.arb, c("Group.1"="sub", "Group.2"="cohort.number", "Group.3"="time"))

cohort.seller.strategy.arb <- mutate(cohort.seller.strategy.arb, prop.IC = IC/count, prop.IR = IR/count)

cohort.seller.strategy.arb$sub <- factor(cohort.seller.strategy.arb$sub)
cohort.seller.strategy.arb$Period <- as.factor(cohort.seller.strategy.arb$time)

tapply(cohort.seller.strategy.arb$prop.IC, cohort.seller.strategy.arb$sub, summary)
tapply(cohort.seller.strategy.arb$prop.IR, cohort.seller.strategy.arb$sub, summary)

dupe.vector <- duplicated(cohort.seller.strategy.arb[,c(1,3,4,5)]) 

cohort.seller.strategy.arb$dupe <- dupe.vector
# check no. duplicate rows within mu for below scaling based on actual number IR/IC choices out of (max) 6
# sort by IR - IC - time - mu then for each consecutive TRUE have 1 dupe

png(filename = "DataAnalysis/cohort.time.IC.IR.png",height=1000,width=2000)  

p <- ggplot(
  cohort.seller.strategy.arb, 
  aes(x = IR, y=IC)
) +
  geom_count(show.legend = FALSE, shape=19) +
  scale_size(range = c(3, 6)) + # adjust as per no. cohorts - current max = 2x - check no. dots
  facet_grid(sub~Period, labeller = labeller(sub = labs1))+
  scale_x_continuous(limits=c(0,6))+
  scale_y_continuous(limits=c(0,6))+
  labs(x = "\n Number of individually rational auction strategies", y = "Number of incentive compatible auction strategies \n")+
  theme_bw() +
  theme(
    strip.text.x = element_text(size = 18),
    strip.text.y = element_text(size = 18),
    panel.border = element_blank(),
    panel.grid.major.y = element_line(color = "grey"),
    panel.grid.major.x = element_line(color = "grey"),
    text = element_text(color = "gray20", size = 16),
    axis.title.x = element_text(face="italic"),
    axis.title.y = element_text(face="italic"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.title=element_blank(),
    legend.text = element_text(size = 16),
    legend.spacing.x = unit(1.0, 'cm'),
    plot.caption = element_text(hjust=0, size = 12),
    plot.title = element_text(size = 16, face = "bold"))
p

dev.off()

# Animation

cohort.seller.strategy.arb$time <- as.integer(cohort.seller.strategy.arb$time)

p <- ggplot(
  cohort.seller.strategy.arb, 
  aes(x = IR, y=IC)
) +
  geom_count(show.legend = FALSE, shape=19) +
  scale_size(range = c(3, 6)) +
  facet_wrap(sub~., labeller = labeller(sub = labs1))+
  scale_x_continuous(limits=c(0,6))+
  scale_y_continuous(limits=c(0,6))+
  labs(x = "\n Number of individually rational auction strategies", y = "Number of incentive compatible auction strategies \n")+
  theme_bw() +
  theme(
    strip.text.x = element_text(size = 18),
    strip.text.y = element_text(size = 18),
    panel.border = element_blank(),
    panel.grid.major.y = element_line(color = "grey"),
    panel.grid.major.x = element_line(color = "grey"),
    text = element_text(color = "gray20", size = 16),
    axis.title.x = element_text(face="italic"),
    axis.title.y = element_text(face="italic"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.title=element_blank(),
    legend.text = element_text(size = 16),
    legend.spacing.x = unit(1.0, 'cm'),
    plot.caption = element_text(hjust=0, size = 12),
    plot.title = element_text(size = 16, face = "bold"))
p

anim <- p + transition_time(time) +
  labs(title = "Period: {frame_time}")

anim_save("DataAnalysis/cohort.time.IC.IR.gif", anim, height=800,width=1200)

# Statistical trend analysis of IC/IR choices over time at sub-treatment level

ts.cohort.seller.strategy.vol <- aggregate(cohort.seller.strategy.vol[c("IC", "IR")], by=list(cohort.seller.strategy.vol$time),
                                           FUN=mean, na.rm=TRUE)

ts.cohort.seller.strategy.arb <- aggregate(cohort.seller.strategy.arb[c("IC", "IR")], by=list(cohort.seller.strategy.arb$sub, cohort.seller.strategy.arb$time),
                                           FUN=mean, na.rm=TRUE)

  # Non-parametric Spearman test: R=number of time the series is/are resampled for a bootstrap test

  # Voluntary

test <- trend.test(ts.cohort.seller.strategy.vol$IC, R=999)
test
test$p.value

test <- trend.test(ts.cohort.seller.strategy.vol$IR, R=999)
test
test$p.value

  # Arbitrator

ts.mu1.IC <- ts(subset(ts.cohort.seller.strategy.arb$IC, ts.cohort.seller.strategy.arb$Group.1 == "mu1"))
test <- trend.test(ts.mu1.IC, R=999)
test
test$p.value

boot.ci(test, conf = 0.95)

ts.mu1.IR <- ts(subset(ts.cohort.seller.strategy.arb$IR, ts.cohort.seller.strategy.arb$Group.1 == "mu1"))
test <- trend.test(ts.mu1.IR, R=999)
test
test$p.value

ts.mu2.IC <- ts(subset(ts.cohort.seller.strategy.arb$IC, ts.cohort.seller.strategy.arb$Group.1 == "mu2"))
test <- trend.test(ts.mu2.IC, R=999)
test
test$p.value

ts.mu2.IR <- ts(subset(ts.cohort.seller.strategy.arb$IR, ts.cohort.seller.strategy.arb$Group.1 == "mu2"))
test <- trend.test(ts.mu2.IR, R=999)
test
test$p.value

## Equilibrium Plots: theory versus actual payoffs as a function of the winning bid ##

seller.strategy <- rbind(seller.strategy.vol, seller.strategy.arb)

lambda.labs <- c("\u03bb = 0.75")
names(lambda.labs) <- c("0.75")

sigma.labs <- c("\u03c3 = 0", "\u03c3 = 1/6", "\u03c3 = 2/6", "\u03c3 = 0.50", "\u03c3 = 4/6", "\u03c3 = 5/6", "\u03c3 = 1")
names(sigma.labs) <- c("0", "0.167", "0.333", "0.5", "0.667", "0.833", "1")

seller.strategy <- mutate(seller.strategy, eqmy=0, eqmx = ifelse(par.sigma == 0, 1.195, ifelse(par.mu < 0.5, 0.795, 0.635)))

winner.strategy <- subset(seller.strategy, winner == 1)

winner.payoff.bid <- aggregate(winner.strategy[c("round.profit", "dSH.a1", "dSL.a1")], by=list(winner.strategy$sub, winner.strategy$strategy.quality, winner.strategy$bid.rescale),
                                                                FUN=mean, na.rm=TRUE)

winner.payoff.bid <- rename(winner.payoff.bid, c("Group.1"="sub", "Group.2"="strategy.quality", "Group.3"="bid.rescale"))

winner.payoff.bid <- mutate(winner.payoff.bid, round.profit.rescale = round.profit/100, theory.profit = ifelse(strategy.quality == 0, dSL.a1, dSH.a1))

winner.payoff.bid <- winner.payoff.bid[,-c(4:6)]

winner.payoff.bid <- mutate(winner.payoff.bid, eqmy=0, eqmx = ifelse(sub == "0" & strategy.quality == 0, 1.195, 
                                                                     ifelse(sub == "mu1" & strategy.quality == 1, 0.795,
                                                                            ifelse(sub == "mu2" & strategy.quality == 1, 0.635, NA))))

# Winning seller dispute profits as a function of bids by quality level: actual versus theory

graph_winner.payoff.bid <- gather(winner.payoff.bid,
                                value = "value",
                                key = "type",
                                round.profit.rescale, theory.profit)

graph_winner.payoff.bid$strategy.quality <- as.factor(graph_winner.payoff.bid$strategy.quality)

# For experiment graph

labs2 <- c("Voluntary", "Arbitrator: \u03bc = 1/3", "Arbitrator: \u03bc = 2/3")
names(labs2) <- c("0", "mu1", "mu2")

labs3 <- c("Low quality", "High quality") 
names(labs3) <- c("0", "1")

# LOESS curve with 95% CI
png(filename = "DataAnalysis/TheoryvsActualPayoffs.png",height = 800, width=1200)  

p <- ggplot(data = graph_winner.payoff.bid) +
  scale_x_continuous(name = expression('b'[i]), breaks = seq(cL,b_max,0.1)) +
  geom_smooth(aes(x = bid.rescale, y=value, linetype=type), colour="black")+
  geom_point(aes(x=eqmx, y=eqmy), shape=9, size=5, colour="red")+
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  scale_linetype_manual(name = "", values = c("dashed","solid"), labels = c("Observed", "Theory")) +
  facet_grid(sub ~ strategy.quality, labeller = labeller(strategy.quality = labs3, sub = labs2))+
  scale_y_continuous(name = expression(pi[S])) + 
  guides(linetype = guide_legend(reverse = TRUE, override.aes = list(size = 0.5, fill=NA))) +
  theme_bw() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    text = element_text(size = 20),
    axis.title.x = element_text(face="italic"),
    axis.title.y = element_text(face="italic"),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.text = element_text(size = 20),
    legend.title=element_text(size=20),
    plot.caption = element_text(hjust=0, size = 12),
    plot.title = element_text(size = 20, face = "bold"))
p

dev.off()

winner.arb.IC <- subset(winner.payoff.bid, sub == "mu1" | sub == "mu2")
winner.arb.IC$sub <- droplevels(winner.arb.IC$sub, exclude = if(anyNA(levels(winner.arb.IC$sub))) NULL else NA)
winner.arb.IC <- winner.arb.IC[,-c(5:7)]
median.bid.low <- c(median(subset(Cohort.Sub$bid.low, Cohort.Sub$sub == "mu1")), 
                    median(subset(Cohort.Sub$bid.low, Cohort.Sub$sub == "mu2")))
winner.arb.IC <- mutate(winner.arb.IC, median.bid.low = median.bid.low[sub]/100)

graph_winner.arb.IC <- gather(winner.arb.IC,
                                  value = "value",
                                  key = "type",
                                  round.profit.rescale)

graph_winner.arb.IC$strategy.quality <- as.factor(graph_winner.arb.IC$strategy.quality)

png(filename = "DataAnalysis/ICActual.png",height = 800, width=1200)  

p <- ggplot(data = graph_winner.arb.IC) +
  scale_x_continuous(name = expression('b'[i]), breaks = seq(cL,b_max,0.1)) +
  geom_smooth(aes(x = bid.rescale, y=value, linetype=strategy.quality), colour="black", se=FALSE)+
  geom_vline(aes(xintercept = median.bid.low), linetype = "dotdash")+
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  scale_linetype_manual(name = "", values = c("solid","dashed"), labels = c(expression(pi[S]('q'^L)),  expression(pi[S]('q'^H)))) +
  facet_wrap(. ~ sub, labeller = labeller(sub = labs2))+
  scale_y_continuous(name = expression(pi[S])) +
  guides(linetype = guide_legend(override.aes = list(size = 0.5, fill=NA))) +
  theme_bw() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    text = element_text(size = 20),
    axis.title.x = element_text(face="italic"),
    axis.title.y = element_text(face="italic"),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.text = element_text(size = 20),
    legend.title=element_text(size=20),
    plot.caption = element_text(hjust=0, size = 12),
    plot.title = element_text(size = 20, face = "bold"))
p

dev.off()

##################################
### Appendix: Buyer regression analysis ###
##################################

# Stata Export [for multi-level modelling ] #

reg.vol.buyer <- subset(Voluntary, role == "Buyer")
reg.mu1.buyer<- subset(Arbitrator, role == "Buyer" & sub == "mu1")
reg.mu2.buyer<- subset(Arbitrator, role == "Buyer" & sub == "mu2")

reg.vol.buyer$risk.1 <- as.numeric(reg.vol.buyer$player.risk1)
reg.vol.buyer$risk.2 <- as.numeric(reg.vol.buyer$player.risk2)
reg.vol.buyer$trust.1 <- as.numeric(reg.vol.buyer$player.trust.index)
reg.vol.buyer$trust.2 <- as.factor(as.numeric(reg.vol.buyer$player.trust_strangers) - 1)
reg.vol.buyer$female <- as.factor(ifelse(reg.vol.buyer$player.gender == "Female", 1, 0))
reg.vol.buyer$econ <- as.factor(reg.vol.buyer$econ)

reg.vol.buyer <- reg.vol.buyer[order(reg.vol.buyer$subject.id, reg.vol.buyer$period),]

reg.mu1.buyer$risk.1 <- as.numeric(reg.mu1.buyer$player.risk1)
reg.mu1.buyer$risk.2 <- as.numeric(reg.mu1.buyer$player.risk2)
reg.mu1.buyer$trust.1 <- as.numeric(reg.mu1.buyer$player.trust.index)
reg.mu1.buyer$trust.2 <- as.factor(as.numeric(reg.mu1.buyer$player.trust_strangers) - 1)
reg.mu1.buyer$female <- as.factor(ifelse(reg.mu1.buyer$player.gender == "Female", 1, 0))
reg.mu1.buyer$econ <- as.factor(reg.mu1.buyer$econ)

reg.mu2.buyer$risk.1 <- as.numeric(reg.mu2.buyer$player.risk1)
reg.mu2.buyer$risk.2 <- as.numeric(reg.mu2.buyer$player.risk2)
reg.mu2.buyer$trust.1 <- as.numeric(reg.mu2.buyer$player.trust.index)
reg.mu2.buyer$trust.2 <- as.factor(as.numeric(reg.mu2.buyer$player.trust_strangers) - 1)
reg.mu2.buyer$female <- as.factor(ifelse(reg.mu2.buyer$player.gender == "Female", 1, 0))
reg.mu2.buyer$econ <- as.factor(reg.mu2.buyer$econ)

reg.mu2.buyer <- reg.mu2.buyer[order(reg.mu2.buyer$subject.id, reg.mu2.buyer$period),]

reg.stata.buyer <- rbind(reg.vol.buyer, reg.mu1.buyer, reg.mu2.buyer)

library(foreign)
write.dta(reg.stata.buyer, "DataAnalysis/Stata/Data/buyer_reg_data.dta")

##################################
### Reference-dependent buyers ###
##################################

### Strategy classification ###

buyer.strategy.arb <- subset(Arbitrator, role == "Buyer")

buyer.strategy.vol <- subset(Voluntary, role == "Buyer")

  # Arbitrator

buyer.strategy.arb <- mutate(buyer.strategy.arb, pA.int = ifelse(sub == "mu2", 
                                                                 pmax(pmin(bid.low, par.mu2*(subsession.value_low+product.quality*(subsession.value_high - subsession.value_low)) + (1-par.mu2)*(subsession.cost_low+product.quality*(subsession.cost_high - subsession.cost_low))), floor), 
                                                                 pmax(pmin(bid.low, par.mu1*(subsession.value_low+product.quality*(subsession.value_high - subsession.value_low)) + (1-par.mu1)*(subsession.cost_low+product.quality*(subsession.cost_high - subsession.cost_low))), floor)))

buyer.strategy.arb <- mutate(buyer.strategy.arb, pA.alt = ifelse(sub == "mu1", 
                                                                 pmax(pmin(bid.low, par.mu2*(subsession.value_low+product.quality*(subsession.value_high - subsession.value_low)) + (1-par.mu2)*(subsession.cost_low+product.quality*(subsession.cost_high - subsession.cost_low))), floor), 
                                                                 pmax(pmin(bid.low, par.mu1*(subsession.value_low+product.quality*(subsession.value_high - subsession.value_low)) + (1-par.mu1)*(subsession.cost_low+product.quality*(subsession.cost_high - subsession.cost_low))), floor)))

buyer.strategy.arb <- mutate(buyer.strategy.arb, lower = ifelse(y == floor, 1, ifelse(is.na(y), NA, 0)), mimic.int = ifelse(y >= pA.int - 2 & y <= pA.int + 2, 1, ifelse(is.na(y), NA, 0)), count = ifelse(!is.na(y), 1, NA), mimic.alt = ifelse(y >= pA.alt - 2 & y <= pA.alt + 2, 1, ifelse(is.na(y), NA, 0)))

buyer.strategy.arb <- mutate(buyer.strategy.arb, above = ifelse(y > pA.int + 2, 1, ifelse(is.na(y), NA, 0)), between = ifelse(y > floor & y <= pA.int - 2, 1, ifelse(is.na(y), NA, 0)))

summary(buyer.strategy.arb$lower)
tapply(buyer.strategy.arb$lower, buyer.strategy.arb$sub, summary)
tapply(subset(buyer.strategy.arb$lower, buyer.strategy.arb$product.quality == 0), subset(buyer.strategy.arb$sub, buyer.strategy.arb$product.quality == 0), summary)
tapply(subset(buyer.strategy.arb$lower, buyer.strategy.arb$product.quality == 1), subset(buyer.strategy.arb$sub, buyer.strategy.arb$product.quality == 1), summary)

summary(buyer.strategy.arb$mimic.int)
tapply(buyer.strategy.arb$mimic.int, buyer.strategy.arb$sub, summary)
tapply(subset(buyer.strategy.arb$mimic.int, buyer.strategy.arb$product.quality == 0), subset(buyer.strategy.arb$sub, buyer.strategy.arb$product.quality == 0), summary)
tapply(subset(buyer.strategy.arb$mimic.int, buyer.strategy.arb$product.quality == 1), subset(buyer.strategy.arb$sub, buyer.strategy.arb$product.quality == 1), summary)

summary(buyer.strategy.arb$mimic.alt)
tapply(buyer.strategy.arb$mimic.alt, buyer.strategy.arb$sub, summary)

summary(buyer.strategy.arb$above)
tapply(buyer.strategy.arb$above, buyer.strategy.arb$sub, summary)

summary(buyer.strategy.arb$between)
tapply(buyer.strategy.arb$between, buyer.strategy.arb$sub, summary)

buyer.strategy.arb <- buyer.strategy.arb[,c(1:8, 20, 22:26, 82:89)]

  # Voluntary

buyer.strategy.vol <- mutate(buyer.strategy.vol, lower = ifelse(y == floor, 1, ifelse(is.na(y), NA, 0)), count = ifelse(!is.na(y), 1, NA))
                                                                 
summary(buyer.strategy.vol$lower)
summary(subset(buyer.strategy.vol$lower, buyer.strategy.vol$product.quality == 0))
summary(subset(buyer.strategy.vol$lower, buyer.strategy.vol$product.quality == 1))

buyer.strategy.vol <- buyer.strategy.vol[,c(1:8, 20, 22:26, 82:83)]

# Lower bound and mimicking choices per individual buyer in Arbitrator.
# Observation per buyer - mu

agg.buyer.strategy.arb <- aggregate(buyer.strategy.arb[c("lower", "mimic.int", "count")], by=list(buyer.strategy.arb$sub, buyer.strategy.arb$subject.id),
                                     FUN=sum, na.rm=TRUE)

agg.buyer.strategy.arb <- rename(agg.buyer.strategy.arb, c("Group.1"="sub", "Group.2"="subject.id"))

agg.buyer.strategy.arb <- mutate(agg.buyer.strategy.arb, prop.lower = lower/count, prop.mimic = mimic.int/count)

agg.buyer.strategy.arb$sub <- factor(agg.buyer.strategy.arb$sub)

tapply(agg.buyer.strategy.arb$prop.lower, agg.buyer.strategy.arb$sub, summary)
tapply(agg.buyer.strategy.arb$prop.mimic, agg.buyer.strategy.arb$sub, summary)

dupe.vector <- duplicated(agg.buyer.strategy.arb[,c(1,3,4)]) 

agg.buyer.strategy.arb$dupe <- dupe.vector
# sort by mimic - lower - mu then for each consecutive TRUE have 1 dupe

png(filename = "DataAnalysis/buyer.strategy.png",height=800,width=800)  

p <- ggplot(
  agg.buyer.strategy.arb, 
  aes(x = lower, y=mimic.int)
) +
  geom_count(show.legend = FALSE, shape=1) +
  scale_size(range = c(3, 12)) + # adjust based on no. buyers with same coordinate [current max = 3x]
  facet_wrap(sub~., nrow=2, labeller = labeller(sub = labs1))+
  scale_x_continuous(limits=c(0,15))+
  scale_y_continuous(limits=c(0,15))+
  labs(x = "\n Number of minimal strategies", y = "Number of mimicking strategies \n")+
  theme_bw() +
  theme(
    strip.text.x = element_text(size = 18),
    strip.text.y = element_text(size = 18),
    panel.border = element_blank(),
    panel.grid.major.y = element_line(color = "grey"),
    panel.grid.major.x = element_line(color = "grey"),
    text = element_text(color = "gray20", size = 16),
    axis.title.x = element_text(face="italic"),
    axis.title.y = element_text(face="italic"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.title=element_blank(),
    legend.text = element_text(size = 16),
    legend.spacing.x = unit(1.0, 'cm'),
    plot.caption = element_text(hjust=0, size = 12),
    plot.title = element_text(size = 16, face = "bold"))
p

dev.off()

# Number of lower bound and mimicking choices over time.

  # Voluntary

buyer.strategy.vol <- mutate(buyer.strategy.vol, time = period)

cohort.buyer.strategy.vol <- aggregate(buyer.strategy.vol[c("lower", "count")], by=list(buyer.strategy.vol$cohort.number, buyer.strategy.vol$time),
                                       FUN=sum, na.rm=TRUE)

cohort.buyer.strategy.vol <- rename(cohort.buyer.strategy.vol, c("Group.1"="cohort.number", "Group.2"="time"))

cohort.buyer.strategy.vol <- mutate(cohort.buyer.strategy.vol, prop.lower = lower/count)

cohort.buyer.strategy.vol$Period <- as.factor(cohort.buyer.strategy.vol$time)

summary(cohort.buyer.strategy.vol$prop.lower)

  # Arbitrator

# 18 observations per cohort - mu

buyer.strategy.arb <- mutate(buyer.strategy.arb, 
                              time = ifelse((order=="12"&sub=="mu1")|(order=="21"&sub=="mu2"), period, period-15))

cohort.buyer.strategy.arb <- aggregate(buyer.strategy.arb[c("lower", "mimic.int", "count")], by=list(buyer.strategy.arb$sub, buyer.strategy.arb$cohort.number, buyer.strategy.arb$time),
                                        FUN=sum, na.rm=TRUE)

cohort.buyer.strategy.arb <- rename(cohort.buyer.strategy.arb, c("Group.1"="sub", "Group.2"="cohort.number", "Group.3"="time"))

cohort.buyer.strategy.arb <- mutate(cohort.buyer.strategy.arb, prop.lower = lower/count, prop.mimic = mimic.int/count)

cohort.buyer.strategy.arb$sub <- factor(cohort.buyer.strategy.arb$sub)
cohort.buyer.strategy.arb$Period <- as.factor(cohort.buyer.strategy.arb$time)

tapply(cohort.buyer.strategy.arb$prop.lower, cohort.buyer.strategy.arb$sub, summary)
tapply(cohort.buyer.strategy.arb$prop.mimic, cohort.buyer.strategy.arb$sub, summary)

dupe.vector <- duplicated(cohort.buyer.strategy.arb[,c(1,3,4,5)]) 

cohort.buyer.strategy.arb$dupe <- dupe.vector
# check no. duplicate rows within mu for below scaling based on actual number lower/mimic choices out of (max) 6
# sort by mimic - lower - time - mu then for each consecutive TRUE have 1 dupe

png(filename = "DataAnalysis/cohort.time.buyer.type.png",height=1000,width=2000)  

p <- ggplot(
  cohort.buyer.strategy.arb, 
  aes(x = lower, y=mimic.int)
) +
  geom_count(show.legend = FALSE, shape=19) +
  scale_size(range = c(3, 9)) + # adjust based on no. cohorts with same coordinate in a period [current max = 3x]
  facet_grid(sub~Period, labeller = labeller(sub = labs1))+
  scale_x_continuous(limits=c(0,6))+
  scale_y_continuous(limits=c(0,6))+
  labs(x = "\n Number of lower bound strategies", y = "Number of mimicking strategies \n")+
  theme_bw() +
  theme(
    strip.text.x = element_text(size = 18),
    strip.text.y = element_text(size = 18),
    panel.border = element_blank(),
    panel.grid.major.y = element_line(color = "grey"),
    panel.grid.major.x = element_line(color = "grey"),
    text = element_text(color = "gray20", size = 16),
    axis.title.x = element_text(face="italic"),
    axis.title.y = element_text(face="italic"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.title=element_blank(),
    legend.text = element_text(size = 16),
    legend.spacing.x = unit(1.0, 'cm'),
    plot.caption = element_text(hjust=0, size = 12),
    plot.title = element_text(size = 16, face = "bold"))
p

dev.off()

# Animation

cohort.buyer.strategy.arb$time <- as.integer(cohort.buyer.strategy.arb$time)

p <- ggplot(
  cohort.buyer.strategy.arb, 
  aes(x = lower, y=mimic.int)
) +
  geom_count(show.legend = FALSE, shape=19) +
  scale_size(range = c(3, 9)) + # adjust as per above
  facet_wrap(sub~., labeller = labeller(sub = labs1))+
  scale_x_continuous(limits=c(0,6))+
  scale_y_continuous(limits=c(0,6))+
  labs(x = "\n Number of lower bound strategies", y = "Number of mimicking strategies \n")+
  theme_bw() +
  theme(
    strip.text.x = element_text(size = 18),
    strip.text.y = element_text(size = 18),
    panel.border = element_blank(),
    panel.grid.major.y = element_line(color = "grey"),
    panel.grid.major.x = element_line(color = "grey"),
    text = element_text(color = "gray20", size = 16),
    axis.title.x = element_text(face="italic"),
    axis.title.y = element_text(face="italic"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.title=element_blank(),
    legend.text = element_text(size = 16),
    legend.spacing.x = unit(1.0, 'cm'),
    plot.caption = element_text(hjust=0, size = 12),
    plot.title = element_text(size = 16, face = "bold"))
p

anim <- p + transition_time(time) +
  labs(title = "Period: {frame_time}")

anim_save("DataAnalysis/cohort.time.buyer.type.gif", anim, height=800,width=1200)

# Non-parametric Spearman tests: R=number of time the series is/are resampled for a bootstrap test

  # Voluntary
  
ts.cohort.buyer.strategy.vol <- aggregate(cohort.buyer.strategy.vol[c("lower")], by=list(cohort.buyer.strategy.vol$time),
                                          FUN=mean, na.rm=TRUE)

test <- trend.test(ts.cohort.buyer.strategy.vol$lower, R=999)
test
test$p.value

  # Arbitrator

ts.cohort.buyer.strategy.arb <- aggregate(cohort.buyer.strategy.arb[c("lower", "mimic.int")], by=list(cohort.buyer.strategy.arb$sub, cohort.buyer.strategy.arb$time),
                                           FUN=mean, na.rm=TRUE)

ts.mu1.lower <- ts(subset(ts.cohort.buyer.strategy.arb$lower, ts.cohort.buyer.strategy.arb$Group.1 == "mu1"))
test <- trend.test(ts.mu1.lower, R=999)
test
test$p.value

ts.mu2.lower <- ts(subset(ts.cohort.buyer.strategy.arb$lower, ts.cohort.buyer.strategy.arb$Group.1 == "mu2"))
test <- trend.test(ts.mu2.lower, R=999)
test
test$p.value

ts.mu1.mimic <- ts(subset(ts.cohort.buyer.strategy.arb$mimic.int, ts.cohort.buyer.strategy.arb$Group.1 == "mu1"))
test <- trend.test(ts.mu1.mimic, R=999)
test
test$p.value

ts.mu2.mimic <- ts(subset(ts.cohort.buyer.strategy.arb$mimic.int, ts.cohort.buyer.strategy.arb$Group.1 == "mu2"))
test <- trend.test(ts.mu2.mimic, R=999)
test
test$p.value

### Observed surplus splits by quality level in Voluntary ###

trade.data.vol <- subset(Voluntary, role == "Buyer" & lost.trade.data == 0 & group.seller_payoff >= 0 & group.buyer_payoff >= 0)

trade.data.vol <- trade.data.vol[,c(8,23,32,33)]
trade.data.vol$product.quality <- as.factor(trade.data.vol$product.quality)

dupe.vector <- duplicated(trade.data.vol[,c(2:4)]) 

trade.data.vol$dupe <- dupe.vector

trade.data.vol$yintercept <- ifelse(trade.data.vol$product.quality == "0", 20, 60)

png(filename = "DataAnalysis/vol.surplus.splits.low.png",height=800,width=800)  

p <- ggplot(subset(trade.data.vol, product.quality == "0"), aes(x=group.buyer_payoff, y=group.seller_payoff)) +
  geom_count(show.legend = FALSE, shape=1) +
  geom_abline(aes(intercept = yintercept, slope = -1))+
  scale_size(range = c(3, 30)) +
  facet_wrap(product.quality~., nrow=1, labeller = labeller(product.quality = labs3))+
  labs(x = "\n Buyer profit", y = "Seller profit \n")+
  scale_x_continuous(limits = c(1,20), breaks=seq(5, 20, 5), labels=seq(5, 20, 5))+
  scale_y_continuous(limits = c(1,20), breaks=seq(5, 20, 5), labels=seq(5, 20, 5))+
  theme_bw() +
  theme(
    strip.text.x = element_text(size = 18),
    strip.text.y = element_text(size = 18),
    panel.border = element_blank(),
    panel.grid.major.y = element_line(color = "grey"),
    panel.grid.major.x = element_line(color = "grey"),
    text = element_text(color = "gray20", size = 16),
    axis.title.x = element_text(face="italic"),
    axis.title.y = element_text(face="italic"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.title=element_blank(),
    legend.text = element_text(size = 16),
    legend.spacing.x = unit(1.0, 'cm'),
    plot.caption = element_text(hjust=0, size = 12),
    plot.title = element_text(size = 16, face = "bold"))
p

dev.off()

png(filename = "DataAnalysis/vol.surplus.splits.high.png",height=800,width=800)  

p <- ggplot(subset(trade.data.vol, product.quality == "1"), aes(x=group.buyer_payoff, y=group.seller_payoff)) +
  geom_count(show.legend = FALSE, shape=1) +
  geom_abline(aes(intercept = yintercept, slope = -1))+
  scale_size(range = c(3, 30)) +
  facet_wrap(product.quality~., nrow=1, labeller = labeller(product.quality = labs3))+
  labs(x = "\n Buyer profit", y = "Seller profit \n")+
  scale_x_continuous(limits = c(1,60), breaks=seq(10, 60, 10), labels=seq(10, 60, 10))+
  scale_y_continuous(limits = c(1,60), breaks=seq(10, 60, 10), labels=seq(10, 60, 10))+
  theme_bw() +
  theme(
    strip.text.x = element_text(size = 18),
    strip.text.y = element_text(size = 18),
    panel.border = element_blank(),
    panel.grid.major.y = element_line(color = "grey"),
    panel.grid.major.x = element_line(color = "grey"),
    text = element_text(color = "gray20", size = 16),
    axis.title.x = element_text(face="italic"),
    axis.title.y = element_text(face="italic"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.title=element_blank(),
    legend.text = element_text(size = 16),
    legend.spacing.x = unit(1.0, 'cm'),
    plot.caption = element_text(hjust=0, size = 12),
    plot.title = element_text(size = 16, face = "bold"))
p

dev.off()

### STATA export for mixture model estimation ###

library(foreign)
write.dta(buyer.strategy.arb, "DataAnalysis/Stata/Data/buyer_arb_data.dta")


