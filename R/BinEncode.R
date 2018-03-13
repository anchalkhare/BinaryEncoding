#'Binary Encoding

#' @param dataframe dataframe with factor/character columns
#' @return dataframe with binary columns
#' @export
#'


Binary_encoding<-function(data1){

  #require(dplyr)
  #require(binaryLogic)

  if(sum(is.na(data1))>0){
    print("Impute missing values in data and then use this function")
    return(data1)
  }else{
    fa_cols<- colnames(data1)[sapply(data1,is.factor)]
    fac_cols<- c(fa_cols,colnames(data1)[sapply(data1,is.character)])
    rm(fa_cols)
    for (i in 1:length(fac_cols)){
      unq<- sort(as.character(unique(unlist(data1[fac_cols[i]]))))
      values<- 0:(length(unq)-1)
      df<- unique(data1[fac_cols[i]])
      if (log(length(unq),2) %%1==0){
        narg=(as.integer(log(length(unq),2)))
      }else{
        narg=(as.integer(log(length(unq),2)+1))
      }
      m<-data.frame(matrix(unlist(as.binary(values, n=narg)),nrow=length(unq),byrow=T))
      colnames(m)<- paste(fac_cols[i],1:narg,sep="")
      m <- data.frame(lapply(m, as.numeric))
      df<- cbind(df,m)
      data1<- left_join(data1,df)
    }
    data1<- data1[,!colnames(data1) %in% fac_cols]
    return(data1)
  }
}
