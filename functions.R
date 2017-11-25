# functions.R created for WineConsumption project


fnDfTransform.USA <- function(x, col.name){
  
  # rename year columns
  names(x)[grep("[0-9].", names(x))] <- gsub("X","",grep("[0-9].", names(x), value=TRUE))
  
  tmp <- subset(x, Country.Code == "USA")
  
  # transpose dataframe
  tmp <- melt(tmp)
  tmp <- subset(tmp, select = c("variable","value"))
  names(tmp) <- c("Year", col.name)
  # remove redundant row
  tmp <- tmp[which(tmp$Year != "X"),]
  
  s = min(as.numeric(as.character(na.omit(tmp$Year)[1]))) - 1934
  tmp.insert <- as.data.frame(cbind(Year=seq(1934,length.out = s),col1=NA))
  # need this line this as line above does not read col.name as a variable
  names(tmp.insert)[2] <- col.name
  
  tmp <- rbind(tmp.insert,tmp)
  
  
  return(tmp)
}


