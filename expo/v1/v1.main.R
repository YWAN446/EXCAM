#load packages;
library(MASS)
library(truncnorm)
library(triangle)
library(igraph)
#load data;
source("~/stat/EXCAM/expo/v1/v1.data1.R")
#load functions
source("~/stat/EXCAM/expo/v1/v1.func.R")

conc <- data.frame(matrix(NA,nrow=1e6,ncol=10))
#simul dists of env.samples e. coli concentration
for (i in 1:length(env.paras)){
  para <- mvrnorm(n.iter,env.paras[[i]][[2]],env.paras[[i]][[3]])
  conc[,i] <- 10^(rnorm(1e6,mean = para[,1], sd = para[,2]))
  names(conc)[i] <- env.paras[[i]][[1]]
}

#truncate conc
# load("~/stat/EXCAM/env_conc/data/max_value.rda")
# max.value1 <- max.value[which(max.value$Group.1=="01" | max.value$Group.2=="soil"),]
# for (i in 1:length(max.value1$x)){
#   conc[which(conc[,max.value1$Group.2[i]]>max.value1$x[i]),max.value1$Group.2[i]] <- max.value1$x[i]
# }
# sapply(conc, max, na.rm = TRUE)


# seq.beh <- gen.beh.seq(1,7,4,14*60)
# 
# comps <- compartments
# behavs <- c(names(rate_table),"bathing","drinking")
# 
# for (i in 1:length(comps)){
#   for (j in 1:length(behavs)){
#     print(paste(comps[i],behavs[j]))
#     print(cbind(expos.by.state(1,1,comps[i],behavs[j],rep(0,7)),
#                                c("mother", "other adult", "other child", "livestock", "fomite", "food", "soil", "mother", "other adult", "other child", "livestock", "fomite", "food", "soil", "bathing water", "drinking water", "breast milk", "areola surface")))
#   }
# }
# 
# expo <- exp.by.cat(1,1,14*60)
# colSums(expo[,11:21],na.rm=TRUE)
