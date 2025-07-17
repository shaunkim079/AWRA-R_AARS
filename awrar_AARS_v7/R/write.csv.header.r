#' Write a csv file with a header
#'
#' Wrapper around the \link{write.table} function to produce a csv file with a header. 
#' The header contains comments added to the data via the \link{comment} function and/or comments passed to the current function via the \code{header} argument.
#'
#' @param data Data frame to be printed
#' @param filename Output file name
#' @param header vector of strings containing headerer information. header can be null, but comment(data) has to be filled up. This is to force user to write comments about its data.
#' @param author File author (default is USER environment variable)
#' @param source.script File author (default is USER environment variable)
#' @param ... Other arguments passed to write.table
#' @return NULL
#' @export
#' @useDynLib awrar
#' @examples
#' d <- data.frame(matrix(rnorm(100),20,5))
#' comment(d) <- c("this is a header 1","this is a header 2","etc...")
#' ff <- "~/d.csv"
#' author <- "Me"
#' write.csv.header(d,file=ff,author=author)
#' 
#' # the mention 'comment.char' has to be used when reading the data
#' dd <- read.csv(ff,comment.char="#") 
write.csv.header <- function(data,filename,header=NULL,
	author=Sys.getenv("USER"),
	source.script="unknown",...){

	# Retrieve comments
	is.dataframe <- length(grep("data\\.frame",class(data)))>0
	cm <- comment(data)
	if(is.dataframe)
	{
		cn <- colnames(data)
		cm.cn <- unlist(mapply(function(i) sprintf("Col %s : %s",
						colnames(data)[i],comment(data[,i])),
						as.list(1:ncol(data))))
		cm <- c(cm,cm.cn)
	}

	# Location of the script
	source.script <- as.character(source.script)

	# Check inputs
	if(length(cm)==0) cm <- NULL
	if(length(cm)!=0) if(paste(cm,collapse="")=="") cm <- NULL
	if(is.null(header) & is.null(cm)) stop("header and comment(data) cannot be NULL")
	header <- unlist(c(header,cm))

	if(class(header)!="character") stop("header is not of character class")
	if(class(author)!="character") stop("author is not of character class")
	if(class(filename)!="character") stop("filename is not of character class")

	if(is.dataframe)
		for(cc in 1:ncol(data))
			if(length(grep("list",class(data[,cc])))>0) data[,cc] <- as.character(data[,cc])

	# Check if file can be written
	err <- try(writeLines("",filename),silent=TRUE)
	if(inherits(err,"try-error")) stop(paste("Cannot write in",filename))

	# Write comments in output file
	commline <- paste("#",paste(rep("-",50),collapse=""),collapse="")
	dimline <- NULL
	if(is.dataframe) 
		dimline <- sprintf("# nrow = %d ncol = %d",nrow(data),ncol(data))

	header2 <- c(commline,"# File comments :",
			dimline,
			sprintf("# %s",header,commline,paste(colnames(data),collapse=",")),
			"#",
			"# File info :",
			sprintf("# Written on : %s",date()),
			sprintf("# Author : %s",author),
			sprintf("# Produced with R script : %s",source.script),			
			sprintf("# R Working dir : %s",getwd()),
			sprintf("# R system : %s",R.version$system),
			sprintf("# R version : %s",R.version$version.string),
			commline,
			paste(colnames(data),collapse=",")
		)
	writeLines(header2,filename)

	# Write output file
	write.table(data,file=filename,append=TRUE,
		sep=",",col.names=FALSE,quote=FALSE,row.names=FALSE,...)		

}
