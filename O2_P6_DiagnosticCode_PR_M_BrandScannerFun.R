#Brand_Scanner


Brand_Scanner <- function(){

#Loaded prefix file
REF.Prefix=read.csv("//192.168.2.32/Group/Data Team/Brand_Update_Location/5_R_Brand_Reference_Files/Brands_Prefix.csv", header = TRUE, row.names=NULL)

Brand_Scanner = data.frame()

###########################################################
###########################################################
###########################################################
###########################################################

for (i in 1:nrow(REF.Prefix)){

message(floor(((nrow(REF.Prefix) - i )/nrow(REF.Prefix))*100), "% Remaining @ ",REF.Prefix[i,1] )

#Create REF.BrandData df, which contains all the prefix information for the BrandName Value
REF.BrandData = data.frame(subset(REF.Prefix, Brand_Folder_Name == as.character(REF.Prefix[i,1])))

#Extract all information about brand from Prefix file in order
BrandName = as.character(REF.BrandData$Brand_Folder_Name)

#Determine the DCI fildername
DCIFolderList = as.character(REF.BrandData$DCI_Internal_1)

#All Possible Inventory Files
vendor_name_1 = as.character(REF.BrandData$vendor_name_1)
vendor_name_2 = as.character(REF.BrandData$vendor_name_2)
vendor_name_3 = as.character(REF.BrandData$vendor_name_3)

###########################################################
###########################################################
###########################################################
###########################################################

#Create Mainsheet location path
BrandFolderLocation = paste("//192.168.2.32/GoogleDrive/Completed Magento Uploads (v 1.0)/",as.character(BrandName), sep = "", collapse = NULL)


#Go to the BrandFolderLocation  location
setwd(BrandFolderLocation)


#Identify the Main--sheet and pull it
PulledMain=read.csv(Sys.glob("main--*.csv"), header = TRUE)

#Determine sku count per mainsheet
Brand_Scanner[i,1] = as.character(BrandName)
Brand_Scanner[i,2] <- nrow(subset(PulledMain, delete=="N" & type=="simple", select=internal_sku))
Brand_Scanner[i,3] <- nrow(subset(PulledMain, delete=="Y" & type=="simple", select=internal_sku))
Brand_Scanner[i,4] <- nrow(subset(PulledMain, delete=="N" & type=="series", select=internal_sku))
Brand_Scanner[i,5] <- nrow(subset(PulledMain, delete=="Y" & type=="series", select=internal_sku))


#Pull out skus
PulledMain$Numb_Sku<- as.character(PulledMain$sku)
PulledMain$MainSheet <- "Y"


###########################################################
###########################################################
###########################################################
###########################################################
#Use the preloaded inventory file

#Subset Invantory File and and combin multiple vendor names into the PulledIF df
IFV1 = subset(InvantoryFile, vendor_name == vendor_name_1)
IFV2 = subset(InvantoryFile, vendor_name == vendor_name_2)
IFV3 = subset(InvantoryFile, vendor_name == vendor_name_3)


#Combin the different vendot names into one file
PulledIF = data.frame()
PulledIF = rbind(PulledIF,IFV1)
PulledIF = rbind(PulledIF,IFV2)
PulledIF = rbind(PulledIF, IFV3)


#Subset the pulledIF by the folling columns
PulledIF <- subset(PulledIF , select = "Numb_Sku")

if(nrow(PulledIF)>0){
PulledIF$InventoryFile <- "Y"
}

#Merge the final pulledIF with the simple match
Main_IF_Merge = merge(PulledMain, PulledIF,  by="Numb_Sku", all = TRUE)


Brand_Scanner[i,6] <- nrow(PulledIF)

if(nrow(PulledIF)>0){
Brand_Scanner[i,7] <- nrow(subset(Main_IF_Merge, InventoryFile =="Y" & is.na(MainSheet), select=Numb_Sku))
Brand_Scanner[i,8] <- floor((as.numeric(Brand_Scanner[i,7])/as.numeric(Brand_Scanner[i,2]))*100)
} else {
	Brand_Scanner[i,7] <- "0"
	Brand_Scanner[i,8] <- "0"
	}

###########################################################
###########################################################
###########################################################
###########################################################
#Find and extract Parts app and merge it with Update.PostIF.Merge

#Create empty Data frame to collect the multiple DCI folder data
PooledPartsApp = data.frame()


#Find the latest file within the specific DCI folder
##Change the directory to temp folder incase any files are outputted
setwd("//192.168.2.32/Group/Data Team/Brand_Update_Location/13. FILE_DUMP")

##Create DCI Folder Path
DCIPath = paste("//192.168.2.32/GoogleDrive/FTP_Downloads/DCI_Files/",as.character(DCIFolderList), sep = "", collapse = NULL)

##Identify the Latest DCI file
LatestDCIFile = sort(list.files(DCIPath , pattern = "*.zip"),decreasing = TRUE)[1]

##Create Path for latest File
LatestDCILocation = paste(as.character(DCIPath) ,as.character(LatestDCIFile), sep = "/", collapse = NULL)

#Find, upzip, remove duplicates and rbind to PooledPartsApp 
##find the Parts App file & Load it
PartsAppFile = paste(strsplit(LatestDCIFile , split='.zip', fixed=TRUE), "_PartVol.txt", sep = "",collapse = NULL)
DCIPartapp= read.table(unzip(LatestDCILocation ,PartsAppFile ), sep ="|", header = TRUE, dec =".", quote = "" , stringsAsFactors=T, fill = TRUE)
	
##Add # Infront of Sku Parts App
DCIPartapp$Numb_Sku = paste("#",as.character(DCIPartapp$EXPPARTNO), sep = "", collapse = NULL)

##Stick DCIPartapp into PooledPartsApp 
PooledPartsApp = rbind(PooledPartsApp, DCIPartapp)

##Trim the PooledPartsApp for necessary information
PooledPartsApp <- subset(PooledPartsApp, select = "Numb_Sku")
PooledPartsApp$DCI <- "Y"

#Merge the PooledPartsApp with Update.PostIF.Merge to create Update.PostPA.Merge
Main_IF_DCI_Merge = merge(Main_IF_Merge, PooledPartsApp,  by=c("Numb_Sku"), all = TRUE)

Brand_Scanner[i,9] <- nrow(PooledPartsApp)
Brand_Scanner[i,10] <- nrow(subset(Main_IF_DCI_Merge, DCI=="Y" & is.na(MainSheet), select=Numb_Sku))
Brand_Scanner[i,11] <- floor((as.numeric(Brand_Scanner[i,10])/as.numeric(Brand_Scanner[i,2]))*100)
Brand_Scanner[i,12] <- LatestDCIFile 

}


names(Brand_Scanner) = c("Brand", "Active_Sku", "Discontinued_Sku", "Active_Series", "Discontinued_Series", "InventoryFile_Sku", "New_IF_Sku", "%New_IF_Sku", "DCI_Sku", "New_DCI_Sku", "%New_DCI_Sku", "DCI_File")

#Record Time of creation
##Create Folder Name with date and Time
	FileName = paste("Brand_Scanner" , gsub(":", "-", as.character(Sys.time())), "csv", sep = ".", collapse = NULL)
	ScannerFileName = paste("//192.168.2.32/Group/Data Team/Brand_Update_Location/12. Brand_Scanning_Output", FileName , sep = "/", collapse = NULL)
	write.csv(Brand_Scanner, file = ScannerFileName , na="")

message("--------------------------*BRAND SCANNER*")
message("")
message("***If you have any issues with the output***")
message("   ***Please Contact Abul Hassan Sheikh***  ")
message("")
message("Version: 1.0")
message("Last Updated: October 11th 2018")
message("Author: Abul Hassan Sheikh")


}


################################################

#Brand_Scanner()

