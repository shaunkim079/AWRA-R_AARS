input_file<-"Q:/work/99_Working/3_Shaun/AWRA_AARS_test_package_v2/inputs/AARSDS/test_input_416047.csv"
date<-as.character(seq(as.Date("2008-01-04"),as.Date("2019-12-31"),by=1))
input<-read.csv(input_file,as.is=T)
new_input<-cbind(date,input)
fn<-gsub("test_input_","test_source_input_",input_file)
write.csv(new_input,fn,quote=F,row.names = F)