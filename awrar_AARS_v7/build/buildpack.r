library(devtools)
library(testthat)
library(roxygen2)
library(RCurl)


if(Sys.info()["sysname"]=="Linux"){
  # setwd("/data/kim079/model_optimisation_framework_v2/packages")
  # setwd("/datasets/work/LW_TVD_MDBA_WORK/8_Working/7_Shaun/data_backup/kim079/model_optimisation_framework_v3/packages")
  setwd("/datasets/work/lw-rq6/work/99_Working/3_Shaun/AWRA_AARS/packages")
  # setwd("/datasets/work/lw-rowra/work/2_Hydrology/0_Working/5_Shaun/transmission_losses/packages")
} else {
  setwd("Q:/work/99_Working/3_Shaun/AWRA_AARS/packages")
}

# uninstall function
source("awrar_AARS_v7/build/uninst.r")

if(is.loaded("awrar_runtimestep")){
  if(Sys.info()["sysname"]=="Linux"){
    dyn.unload("awrar_AARS_v7/src/awrar_AARS_v7.so")
  } else {
    dyn.unload("awrar_AARS_v7/src/awrar_AARS_v7.dll")
  }
}


# Function used to compile the C and Fortran code
compile <-function(pcknm){
  path <- paste(pcknm,"/src",sep="")
  lf <- list.files(path,full.names=TRUE)
  unlink(lf[grep("\\.o$|\\.dll$|\\.so$",lf)],force=TRUE)
  cffiles <- list.files(path,pattern=".*\\.c$|.*\\.f$|.*\\.f90$")
  
  ext <- ".so "                                  
  if(Sys.info()["sysname"]!="Linux") ext <- ".dll "
  
#   cmd <- paste("R CMD SHLIB -o ",path,"/",pcknm,ext,
#                paste(path,"/",cffiles,sep="",collapse=" "),
#                " 2>../scripts/compile.log",collapse="",sep="")
  cmd <- paste("R CMD SHLIB -o ",path,"/",pcknm,ext,
               paste(path,"/",cffiles,sep="",collapse=" "),
               collapse="",sep="")
  
  system(cmd)
  return(cmd)
}


pcknm <- "awrar_AARS_v7"

# Detach and uninstall package if already installe
uninst(pcknm)

compile(pcknm)
# document(pcknm)

lfo <- list.files(pcknm,patter="\\.o$",recursive=TRUE,full.names=TRUE)
unlink(lfo)

# Build  (binaries in windows and sources in :inux)
# build(pcknm,path="../binaries",binary=TRUE) # final build
