#' @title Create Data Partition
#' @description This function returns a train-test dataset
#' @author Alberto Almui?a
#' @param data Dataframe that will be used
#' @param p_train % of data that will be used as train
#' @return
#' Returns a list of length 2 with the train and the test datasets
#' @export
#' @examples
#' data<-create_data_partition(iris, 0.7)


create_data_partition<-function(data, p_train){

        sampled_data<-partial(sample, size = round(p_train*dim(data)[1]))

        index<-do.call(sampled_data, list(1:dim(data)[1]))

        train<-data[index,]; test<-data[-index,]

        return(list(train = train, test = test))
}
