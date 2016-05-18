############################
#### Supercomputer file #2
############################

#####################
# load packages
#####################
packgs<-c("rJava", "glmulti", "lme4")
lapply(packgs, library, character.only=TRUE)

####################
# load data
###################
tree_df<-read.csv("./epb_tree_dataset.csv", header=T)

####################
# create list
# of formulas
####################
yvar.list<-list("dbh", "height", "ratio_hd", "volume") # 3 response variables
xvar<-"classe_drainage+D_index+defoliation+epid_age+light+dist_from_river"# several explanatory variables which will be combined later

####################
# model selection
# analysis
####################

lmer.wrap<-function(formula,data,random="",...){
  lmer(paste(deparse(formula),random),data=data, REML=FALSE,...)
}

par_function <- function(y) {
  name<-paste(y)
  out <- tryCatch({
    glmulti(formula(paste(y, "scale(classe_drainage)+scale(D_index)+scale(defoliation)+scale(epid_age)+scale(light)+scale(dist_from_river)", sep="~")), 
            data=tree_df, random="+(1|loc_id)", fitfunc=lmer.wrap, intercept=TRUE, confsetsize = 10, level=1)
  }, error = function(error) {
    print(paste("ERROR:  ", error))
    return(NA)
  })
  return(out@objects[[1]])
}


