library(tidyverse) # for grammar
library(lubridate) # for working with dates
library(vroom) # To import REALLY large datasets
library(kableExtra)


# Loading data and including only the ten first stock characteristics and two first macro-predictors
raw <- readRDS("data_sample_ass2.rds")
data <- raw[c(1:14,(length(raw)-2):length(raw)-1)]

# Generate the described variables
# Interaction terms
cols <- colnames(data)
len <- length(data)
start <- which(cols=="ret.adj") + 1
end <- which(cols=="b_m") - 1
for (i in start:end) {
  for (j in (end+1):len) {
    name <- paste0("",cols[i],"X",cols[j],"")
    data[name] <- data[,i]*data[,j]
  }
}

# Industry dummies
sorted <- sort(unique(data$sic2))
for (s in sorted) {
  data[paste0("industry",s,"")] <- 0
  idx <- which(data$sic2==s)
  data[idx,paste0("industry",s,"")] <- 1
}


# Plots of macro variables
first_obs <- which(data$permno==data$permno[1])
plot(data$date[first_obs],data$b_m[first_obs],type='l',col="blue",ylab="",xlab="Date")
legend(data$date[first_obs[length(first_obs)-85]],max(data$b_m[first_obs])*1.04,"Blue: Book-to-market ratio\nRed: Long term yield",bty='n')
par(new=TRUE)
plot(data$date[first_obs],data$lty[first_obs],type='l',
     col="red",axes=F,ylab="",xlab="",ylim=c(min(data[first_obs,"lty"])*0.95,max(data[first_obs,"lty"])*1.1))
axis(side=4, at = pretty(range(data[first_obs,"lty"])))
mtext("Yield",side=4,col="black",line=4) 

# Summary by industry
summary_stats <- function(i) {
  df <- data[which(data$sic2==i),c("permno","mktcap","ret.adj")]
  n_firms <- nrow(unique(df["permno"]))
  
  means <- sapply(df[2:3],function(x) round(mean(x),2))
  sds <- sapply(df[2:3],function(x) round(sd(x),2))
  
  final <- data.frame(sic2=i,N=n_firms,mkt_cap_mean=means[1],mkt_cap_sd=sds[1],ret.adj_mean=means[2],ret.adj_sd=sds[2])
  return(final)
}

stat_table <- data.frame(sic2=integer(),N=integer(),mkt_cap_mean=numeric(),mkt_cap_sd=numeric(),
                         ret.adj_mean=numeric(),ret.adj_sd=numeric())

# Apply function to each industry
for (i in sorted) {
  x <- summary_stats(i)
  stat_table <- rbind(stat_table,x)
}
row.names(stat_table) <- NULL

# LaTex table output (copy paste into LaTex)
as.data.frame(stat_table) %>% kable(align = 'c', format = 'latex') 