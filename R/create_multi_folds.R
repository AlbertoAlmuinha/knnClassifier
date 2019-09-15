#' @title Create Multi Folds
#' @description This function creates a resampled dataframe
#' @author Alberto Almui?a
#' @param data Dataframe that will be used
#' @param k Number of sequences created with length(df)/k rows
#' @param times Number of repeats of "create_folds"
#' @return
#' Returns a list with 'k' sequences 'times' times.
#' @export
#' @examples
#' data<-create_multi_folds(iris, 10, 5)


create_multi_folds<-function(data, k = 10, times = 5){

        i<-1

        while(i<times+1){

                folds<-create_folds(data = data, k = k)

                names(folds)<-paste(names(folds),i, sep = "_")

                out<- if(i==1){

                        folds
                } else {

                        c(out, folds)
                }

                i<-i+1
        }

        return(out)
}
