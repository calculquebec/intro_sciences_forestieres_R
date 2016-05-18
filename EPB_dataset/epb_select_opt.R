##################################################################
#####    Model Selection using AIC and EPB dataset           ##### 
#####           V.1 = SOMEWHAT BETTER OPTIONS                #####
##################################################################
# we want to investigate the effect of drainage, simpson's index, balsam fir defoliation by spruce budworm within a variable-radius plot (wedge prism), age of the epidemy, 
# light reaching the middle part of the tree, and distance to the nearest large river on dbh, height, h/d ratio, and volume of white spruce.
# we want to select the best model using AIC
# set working directory
setwd("/path/to/working/directory")

# load packages
packgs<-c("rJava", "glmulti", "lme4")
lapply(packgs, library, character.only=TRUE)

# laod datasets
tree_df<-read.csv("./epb_tree_dataset.csv", header=T)

# INTRODUCING LOOPS AND LISTS
#list of variables
yvar.list<-list("dbh", "height", "ratio_hd", "volume") # 3 response variables
xvar<-"classe_drainage+D_index+defoliation+epid_age+light+dist_from_river"# several explanatory variables which will be combined later
output.list=list()

lmer.wrap<-function(formula,data,random="",...){
  lmer(paste(deparse(formula),random),data=data, REML=FALSE,...)
}

t.st<-Sys.time()
for(y in yvar.list){
  out<-glmulti(formula(paste(y, "scale(classe_drainage)+scale(D_index)+scale(defoliation)+scale(epid_age)+scale(light)+scale(dist_from_river)", sep="~")), 
                data=tree_df, random="+(1|loc_id)", fitfunc=lmer.wrap, intercept=TRUE, confsetsize = 10, level=1)
  output.list[[y]]<-out@objects[[1]]
}
print(t.fin-t.st)


