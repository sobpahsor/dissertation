ClearZeroColumn<-function(a){
  v<-c(0)
  for(i in 4:138){
    x<-sum(a[,i])
    if(x == 0)v<-append(v,i,after = length(v))
  }
  v<-v[-1]
  clearzero<-a[,-v]
  return(data.frame(clearzero))
}

ClearZeroRow<-function(a){
        v<-c(0)
        for(i in 1:21578){
           x<-sum(a[i,4:121])
           if(x == 0)v<-append(v,i,after = length(v))
         }
       v<-v[-1]
       clearzero<-a[-v,]
       return(data.frame(clearzero))
}
