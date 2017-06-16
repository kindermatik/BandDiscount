##  Folder with csv files with data of invoices
##  the folder must have all csv files with invoices for 1 campaign
##  the folder must not have csv files for different campaigns
##
input.folder<-"C:/Users/DuranY/Desktop/Work/BandDiscount/input/C1"

##  csv path for output file
output.folder<-"C:/Users/DuranY/Desktop/Work/BandDiscount/output/C1.csv"

##  New Bands of Discount
##  First vector includes the thresholds
##  Second vector de discount (15% written as 0.15)
thresholds<-c(125,500,1200,4500,20000)
discounts<-c(.15,.30,.33,.35,.37)

####################
####################
##                ##
##      CODE      ##
##                ##
####################
####################


##############################################
##  UPLOAD R LIBRARIES 
##
library(data.table) #upload data.table library

##############################################
##  UPLOAD CSV FILES 
##
raw.data<-data.table()  #creates empty data.table to upload data in csv files
csv.files<-list.files(path=input.folder) #creates a list of all csv files in input folder
##  FOR LOOP
##  uploads files in csv.files
##  Selectes columns:
##  FSC,DocumentNr, Division, Zone, RepAcc, RepCheeck, Units, NTA, BrochurePrice, TransCode,Date 
##  Column numbers to be used to upload the data
##  1,2,3,4,6,7,10,12,14,18,19
for (i in 1:length(csv.files)){
  file.path<-paste(input.folder,csv.files[i],sep="/") #pastes input.folder & element i of list of csv files
  raw.data<-rbind(raw.data,fread(file.path,select = c(1,2,3,4,6,7,10,12,13,18,19),skip=1)) 
}
setnames(raw.data,c("FSC","DocumentNr","Division","Zone","RepAcc","RepCheck"
                      ,"Units","NTA","BrochPrice","TransCode","Date")) #Set column names
setkey(raw.data,DocumentNr)  #sets Document Nr as key of the data.table

##############################################
##  ORDERS DATA 
##
##  Creates column including RepAcc and RepCheck
raw.data[,Rep:=paste(RepAcc,RepCheck,sep="")]

##  Creates column indicating if FSC is Merch. Units (FSC ending in 00)
raw.data[,MerchUnits:=grepl(pattern = "*00",x=FSC)]  #help("grep") for grep function
raw.data[,MerchUnits:=MerchUnits*Units] #Multiplies MerchUnits by Units to get total MerchUnits

##  Removes data with TransCode different than 1
raw.data[,TransCode:=as.integer(TransCode)] #Converts TransCode to integer to make sure filter is 1 (not "001")
raw.data<-raw.data[TransCode==1,] #Only takes rows where TransCode is equal to 1

##  Removes data in Division 99 (employees)
raw.data<-raw.data[Division!=99,] #Only takes rows where Division is not equal to 1

##############################################
##  AGGREGATES DATA 
##
##  Aggregates data by Rep,Division,Zone, BrochPrice
##  Sum of NTA, Merch Units
agg.data<-raw.data[,lapply(.SD,sum,na.rm=TRUE),by="Rep,Division,Zone,BrochPrice"
                   ,.SD=c("NTA","MerchUnits")]

##  Adds column with NPU by Rep
agg.data[,NPU:=BrochPrice/MerchUnits]  #NPU based on Brochure Prices, not NPU in SoS

##  Estimates new discounts based on new thresholds
agg.data[,BrochPrice:=as.numeric(BrochPrice)] #Makes BrochPrice as numeric column 
agg.data[,NTAe:=BrochPrice] #Create Column to include new NTA (NTA estimate)
agg.data[,Threshold:=0] #Column indicating the threshold for the discount
##  applies for assigning discount as far as BrochPrice>=threshold[i]
for(i in 1:length(thresholds)){
  agg.data[BrochPrice>=thresholds[i],NTAe:=BrochPrice*(1-discounts[i])]
  agg.data[BrochPrice>=thresholds[i],Threshold:=i]
}

##############################################
##  ESTIMATE INCREMENTAL SALES
##
##  
inc.unit<-median(agg.data$NPU) #Additional unit brochure price

agg.data[,IPU:=(BrochPrice-NTAe)/MerchUnits]  #Income per Unit

agg.data[,IPU.new:=IPU] #vector to keep previous IPU
agg.data[,unit.new:=0] #vector to keep previous Merch. Units

##  FOR to increase from 5 units to 0 units
for(j in 5:0){
  agg.data[,add.BrochPrice:=j*inc.unit] # Additional Brochure Sales = (j units * inc.unit)
  agg.data[,add.unit:=j]  # Incremental Units
  
  ##  loop to ssgin discounts (same as AGGREGATES DATA section)
  ##  Using additional Brochure Sales
  for(i in 1:length(thresholds)){
    agg.data[(BrochPrice+add.BrochPrice)>=thresholds[i],add.NTAe:=
               (BrochPrice+add.BrochPrice)*(1-discounts[i])]
  }
  
  ##  Calculate IPU with new sales
  agg.data[,IPUTemp:=((BrochPrice+add.BrochPrice)-add.NTAe)/(MerchUnits+(2*add.unit))]
  
  ##  Checks if ITUTemp is higher than IPUnew (IPU from previous iteration)
  agg.data[,extra.unit:=IPUTemp>=IPU.new]
  
  ##  Keeps the number of units that miximises the IPU
  agg.data[,unit.new:=(j*extra.unit)+(unit.new*(1-extra.unit))]
  
  ##  Calculates incremental Brochure Sales
  agg.data[,BrochPrice.new:=unit.new*inc.unit]
  
  ##  Calculates new NTA including new units
  for(i in 1:length(thresholds)){
    agg.data[(BrochPrice+BrochPrice.new)>=thresholds[i],NTA.new:=
               (BrochPrice+BrochPrice.new)*(1-discounts[i])]
  }
  
  ##  Calculates IPU.new including the extra units for Reps that benefit
  agg.data[,IPU.new:=((BrochPrice+BrochPrice.new)-NTA.new)/(MerchUnits+(2*unit.new))]

}

##  Removes the columns not needed
agg.data[,add.BrochPrice:=NULL]
agg.data[,add.unit:=NULL]
agg.data[,add.NTAe:=NULL]
agg.data[,IPUTemp:=NULL]
agg.data[,extra.unit:=NULL]

##  Writes csv file in output folder
write.csv(agg.data,output.folder,row.names = FALSE)