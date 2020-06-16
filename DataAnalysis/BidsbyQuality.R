## Load packages ##
library(tidyverse)
library(psych)

## Additional analysis of bids split by quality level

all.data <- readRDS("Data/DataForAnalysis.rds") 

all.data <- mutate(all.data, bid.low.qL = ifelse(product.quality == 0, bid.low, NA), 
                   bid.low.qH = ifelse(product.quality == 1, bid.low, NA),
                   bid.qL = ifelse(strategy.quality == 0, bid, NA),
                   bid.qH = ifelse(strategy.quality == 1, bid, NA))

agg.bids.quality <- aggregate(all.data[c("bid.qL", "bid.qH")],
                         by=list(all.data$treatment, all.data$cohort.number),
                         FUN=mean, na.rm=TRUE)

agg.bids.quality <- rename(agg.bids.quality, c("Group.1"="treatment", "Group.2"="cohort.number"))

describeBy(agg.bids.quality[,c(3:4)], agg.bids.quality$treatment)

#' Comparison: One-tailed Wilcoxon Signed Rank test for paired sample comparison
df <- subset(agg.bids.quality, treatment == "Voluntary")
wilcox.test(df$bid.qL, df$bid.qH, alternative = c("two.sided"), paired = TRUE) 

df <- subset(agg.bids.quality, treatment == "Arbitrator")
wilcox.test(df$bid.qL, df$bid.qH, alternative = c("two.sided"), paired = TRUE) 
