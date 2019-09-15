#' @title k_knn_tester
#' @description Search the best 'k' parameter
#' @author Alberto Almui?a
#' @param data Dataframe that will be used
#' @param target Integer with the column number of the target variable
#' @param k vector of values to use
#' @param method "bootstrap" or "cross_validation"
#' @param metric Metric that will be evaluated. Possible values: "Accuracy", "Sensitivity" or "Specificity"
#'
#' @return
#' Returns the mean values for the metric selected for each 'k' value and plots
#' @export
#' @examples
#' data<-k_knn_tester(data = iris, target = 5, k = c(3,5,7,9), method = "bootstrap", metric = "Sensitivity")


k_knn_tester<-function(data, target = is.integer(), k = is.vector(), method = c("bootstrap","cross_validation"),
                       metric = c("Accuracy","Sensitivity","Specificity"), ...){

        method<-match.arg(method)

        metric<-match.arg(metric)

        results<-map_dfc(k, function(x){

                switch(method,
                       bootstrap = bootstrap_resampling(data = data, target = target, metric = metric, k = k, ...),
                       cross_validation = k_fold_cross_validation(data = data, target = target, method = "k_fold",
                                                                  k_fold = k, metric = metric, ...)
                )
        })

        names(results)<-k

        if(metric != "Accuracy"){

                rownames(results)<-levels(as.factor(iris[,target]))
        }

        par(mfrow = c(dim(results)[1], 1))

        for(i in 1:dim(results)[1]){

                plot(names(results), results[i,],
                     col = "red",
                     type = "b",
                     pch = 19,
                     main = paste("K parameter vs ", metric, " (", rownames(results)[i],")", sep = ""),
                     xlab = "K Parameter",
                     ylab = paste(metric))

        }

        return(results)
}
