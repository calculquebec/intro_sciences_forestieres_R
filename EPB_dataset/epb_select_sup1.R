###################################################
#### Model Selection using AIC and EPB dataset ####
#### Supercomputer file #1                     ####
###################################################
# cluster creation
###################
library(Rmpi)
library(snow)
library(doSNOW)
library(foreach)

nCore = as.integer(Sys.getenv("MOAB_PROCCOUNT"))
if (is.na(nCore)){ nCore = 8 }

# Multiple nodes
cl = makeMPIcluster(nCore)
registerDoSNOW(cl)

# Single node
#library(doParallel)
#cl<-makeCluster(8)
#registerDoParallel(cl)

# Export variables and libraries
source('./epb_select_sup2.R')
out <- clusterEvalQ(cl, library(rJava))
out <- clusterEvalQ(cl, library(glmulti))
out <- clusterEvalQ(cl, library(lme4))
out <- clusterExport(cl, 'lmer.wrap')
out <- clusterExport(cl, 'glmulti')
out <- clusterExport(cl, 'tree_df')
out <- clusterExport(cl, 'yvar.list')
out <- clusterExport(cl, 'xvar')

####################
# model selection
# analysis
####################

epb.object<-foreach(y=yvar.list, .combine="cbind") %dopar% par_function(y)
saveRDS(epb.object, 'output_epb.rds')

#####################
# delete cluster
#####################
stopCluster(cl)


