#' @title knn Classifier
#' @description Application of knn algorithm for classification
#' @author Alberto Almui?a
#' @param data Dataframe that will be used
#' @param p_train % of rows that will be used as train
#' @param train Dataset used as train set (Only when data and p_train are NULL)
#' @param test Dataset used as test set (Only when data and p_train are NULL)
#' @param k Number of neighbors used
#' @param target Integer with the column number of the target variable
#' @param dist_method Method to calculate the distance between data. Only "Euclidean" available
#' @param probs If TRUE, the results are given in probability. Default is FALSE.
#'
#' @return
#' Returns a list with the original dataset + predicted values and a confusion matrix with several data.
#' @export
#' @examples
#' data<-knn(data = iris, p_train = 0.7, target = 5, k = 3)


knn_classifier<-function(data = NULL, p_train = NULL, train = NULL, test = NULL, k = 3, target = is.numeric(),
                         dist_method = "euclidean", probs = F){

        if(!is.numeric(k)){stop("'p_train' and/or 'k' arguments must be numeric")}

        if(is.null(train) & is.null(test)){

                sampled_data<-partial(sample, size = round(p_train*dim(data)[1]))

                index<-do.call(sampled_data, list(1:dim(data)[1]))
        } else{

                data<-bind_rows(train, test)

                index<-c(1:dim(train)[1])
        }

        if(!is.factor(data[,target])){

                data[,target]<-as.factor(data[,target])

        }

        calc_dist_knn<-function(data, index){

                loc<-apply(data, 2, function(x){

                        which(x %in% index) %>% as.data.frame()
                })

                result<-map_dfc(1:length(loc), function(x){

                        data %>% .[loc[[x]]$.,x] %>% as.data.frame()
                })

                return(result)
        }

        process<-compose(as.data.frame,
                         function(x){x %>% .[2:(k+1),-index]},
                         partial(calc_dist_knn, index = index),
                         partial(apply, MARGIN = 2, FUN = order),
                         full,
                         partial(ecodist::distance, method = dist_method)
        )

        processed<-process(data[,-target])

        values<-data[,target]

        if(probs == F){

                apply_functions<-compose(function(x) {x %>% .[[1]]},
                                         names,
                                         partial(sort, decreasing = T),
                                         table)


                categories<-map_chr(1:(dim(data)[1]-length(index)), function(x){

                        apply_functions(values[as.numeric(processed[,x])])
                })

                test<-data[-index,] %>% mutate(predicted = as.factor(categories))

                conf_matrix<-confusionMatrix(test$predicted, data[-index,target])

                return(list(test= test, conf_matrix = conf_matrix))
        } else{

                apply_functions<-compose(function(x){x %>% .[1]},
                                         as.vector,
                                         function(x){x %>% .[1]/cumsum(as.vector(x))},
                                         partial(sort, decreasing = T),
                                         table)

                predicted_probs<-map_dbl(1:(dim(data)[1]-length(index)), function(x){

                        apply_functions(values[as.numeric(processed[,x])])
                })

                test<-data[-index,] %>% mutate(predicted_probs = predicted_probs)

                multi.roc<-multiclass.roc(ordered(data[-index, target]), test$predicted_probs)

                par(mfrow = c(3,3))

                walk(1:length(multi.roc$rocs), function(x){

                        plot.roc(multi.roc$rocs[[x]], axes = T)
                })

                return(list(test = test, multi.roc = multi.roc))
        }


}
