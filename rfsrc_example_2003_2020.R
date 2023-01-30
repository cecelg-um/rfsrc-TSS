# script for TSS randomforestSRC

# import libraries --------------------------------------------------------
library(randomForestSRC)
library(readxl)
library(writexl)
library(xfun)
library(corrplot)
library(caTools)
library(data.table)
library(parallel)
# set wd ------------------------------------------------------------------
setwd("/scratch/projects/tss_pegasus/")
sink('run_log_20230129_dev_2003_2020.txt')
# parallel set up ? --------------------------------------------------------
# no parallel to avoid exit code 139?
cores <- detectCores()
options(rf.cores=cores, mc.cores=cores)
# open data ----------------------------------------------------------------
a=getwd()
path = '/scratch/projects/tss_pegasus/'
file = 'northern_0.01Deg_18UTC_2003_2020.csv' # ~15GB
print(paste(file))
overalldatunsort <- list()
overalldatunsort <- fread(file.path(path,file))
overalldatunsort <- data.frame(overalldatunsort)
# select outcome -----------------------------------------------------------
outcomes=c("TSS_Katl_std_backedout")
# clean data ---------------------------------------------------------------
weather_sel <- c("fw") # select fair-weather
overalldatunsort <- overalldatunsort[grepl(paste(weather_sel, collapse="|"), overalldatunsort$fw_sw),]
season_sel <- c("WIN") # select winter season
overalldatunsort <- overalldatunsort[grepl(paste(season_sel, collapse="|"), overalldatunsort$season2),]
depth_sel <- c(1.5)    # remove shallow water data
overalldatunsort <- overalldatunsort[!(overalldatunsort$bath < depth_sel),] 
drops <- c("lat", "lon", "time", 'fw_sw', 'season4', "season2", "MSL", "facies_2", "facies_8", "bath", "sfs_Katl_std")
overalldatunsort <- overalldatunsort[ , !(names(overalldatunsort) %in% drops)]
overalldatunsort <- na.omit(overalldatunsort)
# create train ds ---------------------------------------------------------
train <- overalldatunsort
# remove var --------------------------------------------------------------
removevar=c("Rrs_645", 'Rrs_667', 'TSS_Feng_std', "TSS_Katl_std", "TSS_Dorj_std") # list of variables to REMOVE
# random forest -----------------------------------------------------------
rf_overall=list()
holder=which(unlist(lapply(train,class))=='character')
for(z in holder){
  train[,z]=as.factor(train[,z])
}
for(j in 1:(length(outcomes))){ # all response vars
  # for(j in 2){ # only one of them
  print(paste('training', outcomes[j], 'model'))
  rf_overall[[j]]=rfsrc(as.formula(paste0(outcomes[j],'~.')),data=train[,-match(c(removevar, outcomes[-j]), names(train))],importance = T)
}
names(rf_overall)=outcomes # all response vars
setwd(a)
# plotting -------------------------------------------------------------
for(i in 1:length(rf_overall)){
  print(file)
  print(weather_sel)
  print(season_sel)
  print(names(rf_overall)[i])
  print(rf_overall[[i]])
  print(paste0('OOB standardized error:' ,rf_overall[[i]]$err.rate[length(rf_overall[[i]]$err.rate)]/var(rf_overall[[i]]$yvar)))
  png("rf_fig1_2003_2020.png")
  plot(rf_overall[[i]]$importance[order(rf_overall[[i]]$importance,decreasing = T)]/max(rf_overall[[i]]$importance),xaxt='n')
  axis(1,at=1:length(rf_overall[[i]]$importance),names(rf_overall[[i]]$importance)[order(rf_overall[[i]]$importance,decreasing = T)],las=2,main='Relative importance',ylab='VIMP')
  dev.off()
  png("rf_fig2_2003_2020.png")
  plot(rf_overall[[i]],train[,outcomes[i]])
  dev.off()
  # png("partial_plots_dev9.png")
  # par(mar=c(6, 4, 4, 2) + 3)
  # # print('partial plots')
  # plot.variable(rf_overall[[i]],partial=T,plots.per.page = 1, las=2)
  # dev.off()
}
sink()
