get_summary<-function(dataframe)
{
  vmin <- c()
  v1stQ <- c()
  vmed <- c()
  vmean <- c()
  v3rdQ <- c()
  vmax <- c()
  N<-ncol(dataframe)
  for (i in 1:N)
  {
    s<-summary(dataframe[[i]])
    vmin <- append(vmin,s["Min."][[1]])
    v1stQ <- append(v1stQ,s["1st Qu."][[1]])
    vmed <- append(vmed,s["Median"][[1]])
    vmean <- append(vmean,s["Mean"][[1]])
    v3rdQ <- append(v3rdQ,s["3rd Qu."][[1]])
    vmax <- append(vmax,s["Max."][[1]])
  }
  return(as.data.frame(cbind(vmin,v1stQ,vmed,vmean,v3rdQ,vmax)))
}

plot_summary<- function(dataframe)
{
  v <- get_summary(dataframe)
  
  N <- ncol(v)
  par(xpd=TRUE)
  plot(v[[1]],ylim=c(1,350000),col="blue",type="l",lwd = 3)
  lines(v[[2]],col="magenta",lwd = 3)
  lines(v[[3]],col="green",lwd = 3)
  lines(v[[4]],col="black",lwd = 3)
  lines(v[[5]],col="orange",lwd = 3)
  lines(v[[6]],col="red",lwd = 3)
  legend(1,430000,"Min",col="blue",lty=1,lwd=3,box.lwd=0,bg="transparent")
  legend(1,410000,"Max",col="red",lty=1,lwd=3,box.lwd=0,bg="transparent")
  legend(1,390000,"Median",col="green",lty=1,lwd=3,box.lwd=0,bg="transparent")
  
  legend(5,430000,"1st Q.",col="magenta",lty=1,lwd=3,box.lwd=0,bg="transparent")
  legend(5,410000,"Mean",col="black",lty=1,lwd=3,box.lwd=0,bg="transparent")
  legend(5,390000,"3rd Q.",col="orange",lty=1,lwd=3,box.lwd=0,bg="transparent")
}
