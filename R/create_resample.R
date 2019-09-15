#' @title Create Resample
#' @description This function creates a resampled dataframe
#' @author Alberto Almui?a
#' @param data Dataframe that will be used
#' @param times Number of sequences created with random numbers from the original dataset
#' @return
#' Returns a list with 'times' index sequences.
#' @export
#' @examples
#' data<-create_resample(iris, 10)


create_resample<-function(data, times = 10){

        resample<-map(1:times, function(x){

                sample(1:length(data), size = length(data), replace = T)
        })

        names(resample)<-map_chr(1:length(resample), function(x){

                paste("Resample",x, sep = "_")
        })

        return(resample)
}
