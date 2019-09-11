impute<-function(data, sumdata2){
  for(i in 1:nrow(data)){
    if(is.na(data$steps[i])){
      data[i,1]<-sumdata2[which(sumdata2$interval==data[i,3]),2]
    }
  }
  data
}