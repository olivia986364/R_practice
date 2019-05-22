pollutantmean<-function(pollutant,id=1:332){
  dat<-data.frame()da
  dat<-rbind(dat,read.csv(paste(as.character(id),".csv",sep="") ))
  mean(dat[,pollutant],na.rm=TRUE)
}

complete<-function(directory,id=1:322){
  result<-data.frame()
  for(i in id){
    data<- read.csv(paste(as.character(i),".csv",sep=""))
    data_complete<-data[complete.cases(data),]
    num_rows<-nrow(data_complete)
    new_row<-data.frame("id"=i,"nods"=num_rows)
    result<-rbind(result,new_row)
  }
  result
  
corr<-function(id=1:332){
  
  result<-data.frame()
  for(i in id){
    data<- read.csv(paste(as.character(i),".csv",sep=""))
    data_complete<-data[complete.cases(data),]
    relation<-cor(data_complete$sulfate,data_complete$nitrate)
    new_row<-data.frame("id"=i,"cor"=relation)
    result<-rbind(result,new_row)
  }
  result
  
}