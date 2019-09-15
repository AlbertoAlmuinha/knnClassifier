#' @title Create Folds
#' @description This function creates 'k' folds with length(df)/k rows
#' @author Alberto Almui?a
#' @param data Dataframe that will be used
#' @param k Number of sequences created with the rows of the dataset
#' @return
#' Returns a list with 'k' folds
#' @export
#' @examples
#' data<-create_folds(iris, 10)


create_folds<-function(data, k = 10){

        len<-floor(length(data)/k)

        samp<-sample(1:length(data), length(data))

        folds<-map(seq(len,length(data), len), function(x){

                samp[(x-(len-1)):x]
        })

        names(folds)<-map_chr(1:length(folds), function(x){

                paste("Fold",x, sep = "_")
        })

        return(folds)
}
