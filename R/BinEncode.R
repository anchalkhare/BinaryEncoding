#'Binary Encoding

#' @param dataframe dataframe with factor/character columns
#' @return dataframe with binary columns
#' @export
#'
#'Converts factor/character column with n class to binary columns as shown below
#'
#'Input data
#'   Col1
#'    A
#'    B
#'    C
#'    D
#'    
#'Output data
#'   Col1  Col2
#'    0     0       <- A
#'    0     1       <- B
#'    1     0       <- C  
#'    1     1       <- D 
#'    
#'    




Binary_encoding<-function(inputData){
  if(sum(is.na(inputData))>0){
    print("Impute missing values in data and then use this function")
    return(inputData)
  }else{
    tryCatch({
      fa_cols<- colnames(inputData)[sapply(inputData,is.factor)]
      fac_cols<- c(fa_cols,colnames(inputData)[sapply(inputData,is.character)])
      rm(fa_cols)
      for (i in 1:length(fac_cols)){
        unq<- sort(as.character(unique(unlist(inputData[fac_cols[i]]))))
        values<- 0:(length(unq)-1)
        df<- unique(inputData[fac_cols[i]])
        if (log(length(unq),2) %%1==0){
          narg=(as.integer(log(length(unq),2)))
        }else{
          narg=(as.integer(log(length(unq),2)+1))
        }
        m<-data.frame(matrix(unlist(as.binary(values, n=narg)),nrow=length(unq),byrow=T))
        colnames(m)<- paste(fac_cols[i],1:narg,sep="")
        m <- data.frame(lapply(m, as.numeric))
        df<- cbind(df,m)
        inputData<- left_join(inputData,df)
      }
      inputData<- inputData[,!colnames(inputData) %in% fac_cols]
      return(inputData)}
      ,error = function(e){print("Load dependencies dplyr and binaryLogic libraries before running this function as mentioned in documentation")
        return(inputData)}
    )}
}
