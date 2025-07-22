# set-up working directory
wd<-"Q:/work/99_Working/3_Shaun/AWRA_AARS_test_package_v2"
write_dir<-"Q:/work/99_Working/3_Shaun/AWRA_AARS_test_package_v2/inputs"
setwd(wd)

# get available reaches to run
# lf<-list.files("inputs/AARS",pattern="test_input_",full.names = T)
# ids<-gsub("test_input_|.csv","",basename(lf))
# ids

# select an id
id<-"416047" #"416054"

# choose either AARS, AARSDS or DMAARS
model_variant<-"AARS"


# read the files and get config, parameters and input time series
input_file<-paste0("inputs/",model_variant,"/test_input_",id,".csv")
input<-read.csv(input_file,as.is=T)

config_file<-paste0("inputs/",model_variant,"/test_config_",id,".csv")
config<-read.csv(config_file,as.is=T)

parameter_file<-paste0("inputs/",model_variant,"/test_parameters_",id,".csv")
parameters<-read.csv(parameter_file,as.is=T)

riverAlpha<-config$x[config$config_names=="riverAlpha"]
riverBeta<-config$x[config$config_names=="riverBeta"]

riverDepthAlpha<-config$x[config$config_names=="riverDepthAlpha"]
riverDepthBeta<-config$x[config$config_names=="riverDepthBeta"]

dead_storage_length<-config$x[config$config_names=="dead_storage_length"]
dead_storage_max_width<-config$x[config$config_names=="dead_storage_max_width"]
dead_storage_max_height<-config$x[config$config_names=="dead_storage_max_height"]

river_length<-config$x[config$config_names=="river_length"]

# Q<-seq(0,max(input$inflow01)*1.25,length.out=100)

colnames(input)[grep("inflow",colnames(input))]
index_inflows<-grep("inflow",colnames(input))
if(length(index_inflows)>1){
  all_inflow<-rowSums(input[,index_inflows])
}
Q<-quantile(all_inflow,probs=seq(0,1,by=0.001))
# Q<-seq(min(all_inflow),max(all_inflow),length.out=1000)
if(Q[1]!=0) Q<-c(0,Q)
Q<-c(Q,Q[length(Q)]*2)

# rating curve calcs without dead storage #############################################

river_area = riverAlpha*Q^riverBeta
river_width = river_area / river_length
river_depth = (Q/riverDepthAlpha)^(1/riverDepthBeta)

rating_without_ds<-cbind(river_depth,Q*86.4,river_width,rep(0,length(Q)))
# colnames(rating_without_ds)<-c("river_depth_m","discharge_MLperDay","river_width_m","dead_storage_volume_ML")
colnames(rating_without_ds)<-c("Level","Discharge","Width","DeadStorage")


fn<-paste0(write_dir,"/",model_variant,"/rating_without_deadstore_",id,".csv")
write.csv(rating_without_ds,fn,quote = F,row.names = F)
stop()
# dead storage calcs ###############################################################

dead_storage_alpha = 4 * dead_storage_max_height / (dead_storage_max_width^2)
dead_storage_water_max_xsection_area = 2.0 / 3.0 * dead_storage_max_width * dead_storage_max_height;
dead_storage_max_volume = dead_storage_water_max_xsection_area * dead_storage_length

dead_storage_volumes<-seq(0,dead_storage_max_volume,length.out=100)

dead_storage_xsection_area = dead_storage_volumes/dead_storage_length
dead_storage_water_height = (3.0 / 4.0 * dead_storage_xsection_area * dead_storage_alpha^0.5)^(2/3)
dead_storage_water_width = 2.0 * (dead_storage_water_height/dead_storage_alpha)^0.5
dead_storage_surface_area = dead_storage_water_width*dead_storage_length


dead_storage_rating<-cbind(dead_storage_water_height,rep(0,length(dead_storage_volumes)),dead_storage_water_width,dead_storage_volumes/1e3)

# adds the final row to ensure spill
dead_storage_rating<-rbind(dead_storage_rating,
                           c(dead_storage_water_height[length(dead_storage_water_height)]+1e-6,
                             1e-6,
                             dead_storage_water_width[length(dead_storage_water_width)],
                             dead_storage_volumes[length(dead_storage_volumes)]/1e3))

# colnames(dead_storage_rating)<-c("dead_storage_water_height_m","dead_storage_water_discharge_MLperDay",
#                                  "dead_storage_water_width_m","dead_storage_volumes_ML" )
colnames(dead_storage_rating)<-c("Level","Discharge","Width","DeadStorage")


fn<-paste0(write_dir,"/",model_variant,"/rating_deadstore_",id,".csv")
write.csv(dead_storage_rating,fn,quote = F,row.names = F)





