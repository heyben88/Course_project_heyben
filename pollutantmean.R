pollutantmean<-function(directory,pollutant,id=1:332){
  #"C:/Users/pc/Documents/2 SEIMA/Benas/travail/data analysis/R studio files/specdata"
  i=1
  poll_data=vector()
  for (i  in id)
  {if (i<10) 
          {addcar="00"}
          else if (i<100)
                {addcar="0"}
                else {addcar=""}
    
    data_i=read.csv(paste(directory,"/",addcar,i,".csv",sep=""))
    data_pol=data_i[pollutant]
    data_ss_na=data_pol[!is.na(data_pol)]
      poll_data=c(poll_data,data_ss_na)
  }
  mean(poll_data)
}


complete<-function(directory,id=1:332){
  #"C:/Users/pc/Documents/2 SEIMA/Benas/travail/data analysis/R studio files/specdata"
  i=1
  k=1
  numb_each_full=vector()
  for (i  in id)
  {k=k+1
  if (i<10) 
    {addcar="00"}
    else if (i<100)
            {addcar="0"}
             else {addcar=""}
  data_i=read.csv(paste(directory,"/",addcar,i,".csv",sep=""))
  
  data_pol=data_i[1]
  listOK=!is.na(data_pol)
  for (j in 2:dim(data_i)[2]){
      listOK=listOK & !is.na(data_i[j])
  }
  numb_each_full=rbind(numb_each_full,c(i,length(data_pol[listOK])))
  
  }
  numb_each_full 
  
}


library(data.table) 
corr<-function(directory,threshold=0,id=1:332){
  size_of_good_value=complete(directory)
  print(size_of_good_value)
  i=1
  sol<-vector()
  for (i in id)
    {if(size_of_good_value[i,2]>threshold)
          { print(c("i",i))
            print(size_of_good_value[1,2])
            if (i<10) 
                {addcar="00"}
                else if (i<100)
                {addcar="0"}
                else {addcar=""}
            data_i=read.csv(paste(directory,"/",addcar,i,".csv",sep=""))
            data_pol=data_i[2:3]
            listOK=!is.na(data_pol[1])& !is.na(data_pol[2])
            data_propre_i=data_pol[listOK]
            if(length(data_propre_i)>0)
              {dim(data_propre_i)<-c(length(data_propre_i)/2,2)
               sol<- c(sol,cor(data_propre_i[,1],data_propre_i[,2]))}
            }
  }
  print(sol)
  c(min(sol),median(sol),mean(sol),max(sol))
}