#' @title Bootsrap Resampling
#' @description Bootsrap Resampling for knn Classifier algorithm
#' @author Alberto Almui?a
#' @param data Dataframe that will be used
#' @param target Integer with the column number of the target variable
#' @param times Number of times to create resamples
#' @param algorithm Only "knn" available
#' @param metric Metric that will be evaluated. Possible values: "Accuracy", "Sensitivity" or "Specificity"
#'
#' @return
#' Returns the mean values for the metric selected
#' @export
#' @examples
#' data<-boostrap_resampling(data = iris, target = 5, times = 10, metric = "Sensitivity")


bootstrap_resampling<-function(data, target, times = 10, algorithm = "knn", metric = "Accuracy", ...){

        samples<-create_resample(data[,target], times = times)

        if(algorithm=="knn"){

                predicted<-lmap(1:times, function(x){

                        knn_classifier(train = data[samples[[x]],], test = data[-samples[[x]],], target = target)
                })
        }

        loc<-which(names(predicted)=="conf_matrix")

        info<-lmap(loc, function(x){

                list(predicted[[x]])

        })

        result<-map_dfc(1:times, function(x){

                switch(metric,
                       Accuracy = info[[x]]$overall[1],
                       Sensitivity = info[[x]]$byClass[,"Sensitivity"],
                       Specificity = info[[x]]$byClass[,"Specificity"])
        }) %>% as.data.frame()

        rownames(result)<-switch(metric,
                                 Accuracy = names(info[[1]]$overall[1]),
                                 Sensitivity = names(info[[1]]$byClass[,"Sensitivity"]),
                                 Specificity = names(info[[1]]$byClass[,"Specificity"]))

        metric_mean<-apply(result, 1, mean)


        return(metric_mean)
}
