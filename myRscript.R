pollutantmean<-function(directory,pollutant,id=1){
  #"C:/Users/pc/Documents/2 SEIMA/Benas/travail/data analysis/R studio files/specdata"
  i=1
  poll_data=vector()
  for (i  in id)
  {if (i<10) 
          {addcar="00"}
          else if (i<100)
                {addcar="0"}
                else {addcar=""}
    
    data_i=read.table(paste(directory,"/",addcar,i,".csv",sep=""))
      poll_data=c(poll_data,data_i[pollutant])
  }
}
