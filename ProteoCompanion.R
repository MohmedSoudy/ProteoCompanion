library(dplyr)
#Used for getting arguments from user 
args = commandArgs(trailingOnly=TRUE)
MatchedList <- as.list(as.numeric(strsplit(args[2], ",")[[1]]))
"Get Indecies of each Selection"
AggIndecies <- which(MatchedList %in% 0)
ConcIndecies <- which(MatchedList %in% 1)
NotappIndecies <- which(MatchedList %in% 2) #To be kept without changes
NotIncIndecies <- which(MatchedList %in% 3)   #To be deleted from final output
AggRoundIndecies <- which(MatchedList %in% 4)
MatchedList <<- list(AggIndecies , ConcIndecies , NotappIndecies , NotIncIndecies , AggRoundIndecies)
MatchedList <- lapply(MatchedList , FUN = function(x) x<-x+2) #2 for protein report
AllSubReportContent <- NULL
"Start Processing of data"
for (subdir in list.dirs(path = args[1], full.names = TRUE,recursive=FALSE)){
  #ReportContent <- NULL 
  for(file in list.files(subdir)){
    ReportsPath <<- paste0(subdir , '\\' , file)
    ReportContent <<- read.csv(ReportsPath , sep = "\t")
    SortedReportContent <<- ReportContent%>%arrange(ReportContent[,2] ,-ReportContent[,7])
    DupliactedIndecies <<- which(duplicated(SortedReportContent[,2]))
    if (is.na(DupliactedIndecies[1]))
      DistincitReportContent <<- SortedReportContent
    else
      DistincitReportContent <<- SortedReportContent[-DupliactedIndecies,]
    AllSubReportContent <<- rbind(AllSubReportContent,DistincitReportContent)
  }
  AllSortedReportContent <<- AllSubReportContent%>%arrange(AllSubReportContent[,2] ,-AllSubReportContent[,7])
  AvgCOlnames <<- colnames(AllSortedReportContent)[MatchedList[[1]]]
  ConcColnames <<- colnames(AllSortedReportContent)[MatchedList[[2]]]
  NotappColnames <<- colnames(AllSortedReportContent)[MatchedList[[3]]]
  TobedeletedColnames <<- colnames(AllSortedReportContent)[MatchedList[[4]]]
  AvgRoundedColnames <<- colnames(AllSortedReportContent)[MatchedList[[5]]]
  AvgeragedReportContent <<- aggregate(AllSortedReportContent[,AvgCOlnames], list(MainAccesion =AllSortedReportContent[,2]), FUN = mean)
  AvgeragedRoundedReportContent <<- aggregate(AllSortedReportContent[,AvgRoundedColnames], list(MainAccesion =AllSortedReportContent[,2]) , FUN = mean)
  AvgeragedRoundedReportContent[,-1] <-ceiling(AvgeragedRoundedReportContent[,-1])  
  ConcReportContent <<- setNames(aggregate(AllSortedReportContent[,ConcColnames],
                                           by = list(MainAccesion=AllSortedReportContent[,2]),
                                           paste, collapse="$"),c(colnames(AllSortedReportContent)[2],ConcColnames))
  
  ConcReportContent[ConcReportContent == "$"] <- " "
  ConcReportContent[ConcReportContent == "NA"] <- " "
  ConcReportContent[ConcReportContent == "$NA"] <- " "
  ConcReportContent[ConcReportContent == "NA$"] <- " "
  
  NotappReportContent <<- aggregate(AllSortedReportContent[,NotappColnames],list(MainAccesion =AllSortedReportContent[,2]), FUN = function(a) a[1])

  PreReportContent <<- merge(AvgeragedReportContent , AvgeragedRoundedReportContent , by = 1)
  PreReportContent <<- merge(PreReportContent , ConcReportContent , by = 1)
  FinalReportContent <<- merge(PreReportContent , NotappReportContent , by = 1)
  FinalReportContent<-FinalReportContent %>%
    mutate_all(as.character)
  
  OutputSplitted <- strsplit(subdir , '/')
  OutputName <- OutputSplitted[[1]][length(OutputSplitted[[1]])]
  OutputPath <- paste0(subdir , '\\' , paste0(OutputName , '.csv'))
  
  write.csv(FinalReportContent, OutputPath)
  print("Done")
  ReportContent <- NULL 
  AllSubReportContent <- NULL
}