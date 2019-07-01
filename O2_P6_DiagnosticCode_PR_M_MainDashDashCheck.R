################################################
#Load the list of Main Sheet folder Names
#Loaded prefix file

MainDashDashCheck <- function(){


REF.Prefix = read.csv("//192.168.2.32/Group/Data Team/Brand_Update_Location/5_R_Brand_Reference_Files/Brands_Prefix.csv", header = TRUE, row.names=NULL)

for (i in 1:nrow(REF.Prefix)){
	BrandFolderLocation = paste("//192.168.2.32/GoogleDrive/Completed Magento Uploads (v 1.0)/",as.character(REF.Prefix[i,1]), sep = "", collapse = NULL)
	setwd(BrandFolderLocation)

	#Identify the Main--sheet and pull it
	x <- Sys.glob("main--*.csv")
	
	#message(i)
	#message(REF.Prefix[i,1])
	if(length(x)>1){
		message("ERROR: ", REF.Prefix[i,1], " : ", x) 
	}

}
message("")
message("Done")
message("")
message("Version: 1.0")
message("Last Updated: January 23th 2019")
message("Author: Abul Hassan Sheikh")
}
###########
#MainDashDashCheck()