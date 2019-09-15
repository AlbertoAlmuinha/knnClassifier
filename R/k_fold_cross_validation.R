#' @title k-Fold Cross Validation
#' @description k-Fold Cross Validation for knn Classifier algorithm
#' @author Alberto Almui?a
#' @param data Dataframe that will be used
#' @param target Integer with the column number of the target variable
#' @param method Could be "k_fold" or "multi_k_fold"
#' @param algorithm Only "knn" available
#' @param k_fold Number of folds that will be created
#' @param times Number of times to create "multi_k_fold"
#' @param metric Metric that will be evaluated. Possible values: "Accuracy", "Sensitivity" or "Specificity"
#'
#' @return
#' Returns the mean values for the metric selected
#' @export
#' @examples
#' data<-k_fold_cross_validation(data = iris, target = 5, method = "k_fold", k_fold = 3, metric = "Sensitivity")


k_fold_cross_validation<-function(data, target, method = c("k_fold", "multi_k_fold") ,algorithm = "knn", k_fold = 10, times = 5,
                                  metric = "Accuracy", ...){

        method<-match.arg(method)

        folds<-switch(method,
                      k_fold = create_folds(data[,target], k = k_fold),
                      multi_k_fold = create_multi_folds(data[,target], k = k_fold, times = times)
        )

        if(algorithm=="knn"){

                predicted<-lmap(1:k_fold, function(x){

                        knn_classifier(train = data[folds[[x]],], test = data[-folds[[x]],], target = target)
                })
        }

        loc<-which(names(predicted)=="conf_matrix")

        info<-lmap(loc, function(x){

                list(predicted[[x]])

        })

        result<-map_dfc(1:k_fold, function(x){

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
