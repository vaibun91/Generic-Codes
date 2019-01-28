library(reshape2)

Summary_Function <- function(data){
  
  all_columns <- colnames(data)
  nums_cont <- sapply(data,is.numeric)
  cont_columns <-colnames(data)[nums_cont]
  nums_categ <- sapply(data,is.character)
  categ_columns <- colnames(data)[nums_categ]
  
  print("1")
  
  for(colname in categ_columns){data[,colname] <- as.factor(data[,colname])}
  nums_categ <- sapply(data,is.factor)
  categ_columns <- colnames(data)[nums_categ]
  
  print("2")

  data_categ <- data[,sapply(data,function(x) !all(is.na(as.Date(as.character(x),format = "%Y-%m-%d"))))]
  date_columns <- colnames(date_categ)
  
  for(i in date_columns)
  {
    data[,i] <- as.Date(data[,i],format ="%Y-%m-%d")
    
  } 
  categ_columns <- categ_columns[which(!(categ_columns %in% date_columns))]
  
  print("3")

  "finding number of zeroes in continuous variables"
  
  temp1 <- NULL
  for(i in cont_columns){temp1[i]<-sum(data[,i]==0,na.rm = TRUE)}
  
  NoOFZeroes <- data.frame(temp1)
  write.csv(NoOFZeroes,"NoOFZeroes.csv")
  
  print("4")
  
  "Finding the number of levels in categ_columns"
  
  categ_columns_info_func <- function(i){return(paste(i,class(i),sum(is.na(data[,i])),sum(is.na(data[,i]))*100/nrow(data),length(levels(data[,i])),sep=";"))}
  
  categ_columns_info <-data.frame(Variable=character())
  for(i in categ_columns){
    temp <- NULL
    temp <- as.data.frame(categ_columns_info_func(i))
    colnames(temp)[1]<-"Variable"
    categ_columns_info <- rbind(categ_columns_info,temp)
  }  
  
  print("5")
  
  
  nume_columns_info_func <- function(i){return(paste(i,"Numeric",sum(is.na(data[,i])),sum(is.na(data[,i]))*100/nrow(data),"NA",sep=";"))}
  nume_columns_info <- data.frame(Variable = character())
  
  for(i in cont_columns){
    temp <- NULL
    temp <- as.data.frame(nume_columns_info_func(i))
    colnames(temp)[1]<- "Variable"
    nume_columns_info <- rbind(nume_columns_info,temp)
  }
  
  
  print("6")
  
  uniAnalysisCont<-function(var)
  {
    
    Pctl_tbl <- as.vector(quantile(data[,var],probs = c(.01,.10,.20,.50,.80,.90,.99,1.0),na.rm = TRUE))
    Pctl_tbl <- data.frame(c("P001","P010","P020","P050","P080","P090","P099","P100"),Pctl_tbl)
    Pctl_tbl <- data.frame(c(var,var,var,var),Pctl_tbl)
    colnames(Pctl_tbl)<-c("variable","quantiles","values")
    return(Pctl_tbl)
  }  
  
  uniDataCont <- data.frame(variable=character(),quantiles=character(),values=numeric())
  
  for(i in 1:length(cont_columns)){
    uniDataCont<-rbind(uniDataCont,uniAnalysisCont(cont_columns[i]))
  }
  
  uniDataCont <- dcast(uniDataCont,variable ~quantiles)
  
  nume_columns_info <- cbind(nume_columns_info,uniDataCont) 
  
  print("7")
  
  date_columns_info_func <- function(i){return(paste(i,"Date",sum(is.na(data[,i])),sum(is.na(data[,i]))*100/nrow(data),"NA",sep=";"))}
  
  date_columns_info <- data.frame(Variable=character())
  
  for(i in date_columns){
    temp <- NULL
    temp <- as.data.frame(date_columns_info_func(i))
    colnames(temp)[1]<-"Variable"
    date_columns_info <- rbind(date_columns_info,temp)
  }
  
  print("8")
  
  categ_columns_info <- cbind(categ_columns_info,"P001","P010","P020","P050","P080","P090","P099","P100")
  
  if(nrow(date_columns_info)>0){date_columns_info<-cbind(date_columns_info,"P001","P010","P020","P050","P080","P090","P099","P100")}
    
  Total_info<-rbind(categ_columns_info,date_columns_info)
  colnames(Total_info)[2:length(Total_info)]<-c("P001","P010","P020","P050","P080","P090","P099","P100")
  
  print("9")

  for (i in c("P001","P010","P020","P050","P080","P090","P099","P100")){
    Total_info[,i] <- " "
  }   
  
  print("10")
  
  Total_info <- rbind(nume_columns_info[,-c(2)],Total_info)
  
  print("11")
  
  library(dplyr)
  library(tidyr)
  
  Final_out <- Total_info %>%
    seperate(Variable,c("Variable","Type","No Of Missing","Percentage of Missing","No of Levels"),";")
  
  print("12")
  
  write.csv(Final_out,"Summary_MedPro_09_07.csv",row.names = FALSE)

  }