##################################################################
#####     Model Selection using AIC and EPB dataset          ##### 
#####           V.1 = A BADLY WRITTEN SCRIPT                 #####
##################################################################
# we want to investigate the effect of drainage, simpson's index, balsam fir defoliation by spruce budworm within variable-radius plot (wedge prism), age of the epidemy, 
# light reaching the middle part of the tree, and distance to the nearest large river on dbh, height, h/d ratio, and volume of white spruce.
# we want to select the best model using AIC

# ***** THIS IS A VERY BAD WAY TO DO IT:

# set working directory
setwd("/path/to/working/directory")

# load packages
library(lme4)

# laod datasets
tree_df<-read.csv("./epb_tree_dataset.csv", header=T)

# 1. DBH
test=lmer(dbh~(1|type), data = tree_df)
AIC(test)
# AIC is 958.1
test=lmer(dbh~classe_drainage+(1|type), data = tree_df)
AIC(test)
# AIC is 802.7
test=lmer(dbh~D_index+(1|type), data = tree_df)
AIC(test)
# AIC is 867.3
test=lmer(dbh~defoliation+(1|type), data = tree_df)
AIC(test)
# AIC is 960.1
test=lmer(dbh~epid_age+(1|type), data = tree_df)
AIC(test)
# AIC is 958.1
test=lmer(dbh~light+(1|type), data = tree_df)
AIC(test)
# AIC is 960
test=lmer(dbh~dist_from_river+(1|type), data = tree_df)
AIC(test)
# AIC is 958.7
test=lmer(dbh~classe_drainage+D_index+(1|type), data = tree_df)
AIC(test)
# AIC is 776.7
test=lmer(dbh~defoliation+D_index+(1|type), data = tree_df)
AIC(test)
# AIC is 869.2
test=lmer(dbh~epid_age+D_index+(1|type), data = tree_df)
AIC(test)
# AIC is 958.1
test=lmer(dbh~light+D_index+(1|type), data = tree_df)
AIC(test)
# AIC is 958.1
test=lmer(dbh~dist_from_river+D_index+(1|type), data = tree_df)
AIC(test)
# AIC is 958.1
test=lmer(dbh~classe_drainage+defoliation+(1|type), data = tree_df)
AIC(test)
# AIC is 958.1
test=lmer(dbh~D_index+defoliation+(1|type), data = tree_df)
AIC(test)
# AIC is 958.1
test=lmer(dbh~epid_age+defoliation+(1|type), data = tree_df)
AIC(test)
# AIC is 960.1
test=lmer(dbh~light+defoliation+(1|type), data = tree_df)
AIC(test)
# AIC is 962
test=lmer(dbh~dist_from_river+defoliation+(1|type), data = tree_df)
AIC(test)
# AIC is 960.6

# the list goes on, I'm not even going to finish it...

# 2. DBH
test=lmer(height~(1|type), data = tree_df)
AIC(test)
# AIC is 2007
test=lmer(height~classe_drainage+(1|type), data = tree_df)
AIC(test)
# AIC is 2005
test=lmer(height~D_index+(1|type), data = tree_df)
AIC(test)
# AIC is 2005
test=lmer(height~defoliation+(1|type), data = tree_df)
AIC(test)
# AIC is 2008
test=lmer(height~epid_age+(1|type), data = tree_df)
AIC(test)
# AIC is 2007
test=lmer(height~light+(1|type), data = tree_df)
AIC(test)
# AIC is 2008
test=lmer(height~dist_from_river+(1|type), data = tree_df)
AIC(test)
# AIC is 2009
test=lmer(height~classe_drainage+D_index+(1|type), data = tree_df)
AIC(test)
# AIC is 1996
test=lmer(height~defoliation+D_index+(1|type), data = tree_df)
AIC(test)
# AIC is 2007
test=lmer(height~epid_age+D_index+(1|type), data = tree_df)
AIC(test)
# AIC is 2005
test=lmer(height~light+D_index+(1|type), data = tree_df)
AIC(test)
# AIC is 2006
test=lmer(height~dist_from_river+D_index+(1|type), data = tree_df)
AIC(test)
# AIC is 2007
test=lmer(height~classe_drainage+defoliation+(1|type), data = tree_df)
AIC(test)
# AIC is 2007
test=lmer(height~D_index+defoliation+(1|type), data = tree_df)
AIC(test)
# AIC is 2007
test=lmer(height~epid_age+defoliation+(1|type), data = tree_df)
AIC(test)
# AIC is 2008
test=lmer(height~light+defoliation+(1|type), data = tree_df)
AIC(test)
# AIC is 2010
test=lmer(height~dist_from_river+defoliation+(1|type), data = tree_df)
AIC(test)
# AIC is 2010

# 3. ratio_hd
test=lmer(ratio_hd~(1|type), data = tree_df)
AIC(test)
# AIC is 519.3
test=lmer(ratio_hd~classe_drainage+(1|type), data = tree_df)
AIC(test)
# AIC is 514
test=lmer(ratio_hd~D_index+(1|type), data = tree_df)
AIC(test)
# AIC is 485.5
test=lmer(ratio_hd~defoliation+(1|type), data = tree_df)
AIC(test)
# AIC is 521.1
test=lmer(ratio_hd~epid_age+(1|type), data = tree_df)
AIC(test)
# AIC is 519.3
test=lmer(ratio_hd~light+(1|type), data = tree_df)
AIC(test)
# AIC is 520.2
test=lmer(ratio_hd~dist_from_river+(1|type), data = tree_df)
AIC(test)
# AIC is 520.1
test=lmer(ratio_hd~classe_drainage+D_index+(1|type), data = tree_df)
AIC(test)
# AIC is 487.5
test=lmer(ratio_hd~defoliation+D_index+(1|type), data = tree_df)
AIC(test)
# AIC is 487
test=lmer(ratio_hd~epid_age+D_index+(1|type), data = tree_df)
AIC(test)
# AIC is 485.5
test=lmer(ratio_hd~light+D_index+(1|type), data = tree_df)
AIC(test)
# AIC is 484.9
test=lmer(ratio_hd~dist_from_river+D_index+(1|type), data = tree_df)
AIC(test)
# AIC is 486.8
test=lmer(ratio_hd~classe_drainage+defoliation+(1|type), data = tree_df)
AIC(test)
# AIC is 515.7
test=lmer(ratio_hd~D_index+defoliation+(1|type), data = tree_df)
AIC(test)
# AIC is 487
test=lmer(ratio_hd~epid_age+defoliation+(1|type), data = tree_df)
AIC(test)
# AIC is 522
test=lmer(ratio_hd~light+defoliation+(1|type), data = tree_df)
AIC(test)
# AIC is 522
test=lmer(ratio_hd~dist_from_river+defoliation+(1|type), data = tree_df)
AIC(test)
# AIC is 522


# 4. volume
test=lmer(volume~(1|type), data = tree_df, REML=F)
AIC(test)
# AIC is 2113
test=lmer(volume~classe_drainage+(1|type), data = tree_df)
AIC(test)
# AIC is 1786
test=lmer(volume~D_index+(1|type), data = tree_df)
AIC(test)
# AIC is 2114
test=lmer(volume~defoliation+(1|type), data = tree_df)
AIC(test)
# AIC is 958.1
test=lmer(volume~epid_age+(1|type), data = tree_df)
AIC(test)
# AIC is 2113
test=lmer(volume~light+(1|type), data = tree_df)
AIC(test)
# AIC is 2112
test=lmer(volume~dist_from_river+(1|type), data = tree_df)
AIC(test)
# AIC is 2113
test=lmer(volume~classe_drainage+D_index+(1|type), data = tree_df)
AIC(test)
# AIC is 1777
test=lmer(volume~defoliation+D_index+(loc/type), data = tree_df)
AIC(test)
# AIC is 2030
test=lmer(volume~epid_age+D_index+(loc/type), data = tree_df)
AIC(test)
# AIC is 2028
test=lmer(volume~light+D_index+(loc/type), data = tree_df)
AIC(test)
# AIC is 2029
test=lmer(volume~dist_from_river+D_index+(loc/type), data = tree_df)
AIC(test)
# AIC is 2029
test=lmer(volume~classe_drainage+defoliation+(loc/type), data = tree_df)
AIC(test)
# AIC is 1788
test=lmer(volume~D_index+defoliation+(loc/type), data = tree_df)
AIC(test)
# AIC is 2030
test=lmer(volume~epid_age+defoliation+(loc/type), data = tree_df)
AIC(test)
# AIC is 2114
test=lmer(volume~light+defoliation+(loc/type), data = tree_df)
AIC(test)
# AIC is 2114
test=lmer(volume~dist_from_river+defoliation+(loc/type), data = tree_df)
AIC(test)
# AIC is 2115