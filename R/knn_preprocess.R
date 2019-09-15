#' @title knn Preprocess
#' @description This function applies step functions of recipes package
#' @author Alberto Almui?a
#' @param formula Formula that will be evaluated in knn algorithm (for example: Species ~ .)
#' @param data Dataframe that will be modified
#' @param methods Steps functions of the recipes package to be applied. For example( c("step_center", "step_scale", "step_range"))
#' @param apply_on "Categorical" or "Numeric". The column type in which step functions will be applied.
#' @param role "Predictor" or "Outcome". Outcome is the target variable and "Predictor" represent all the rest in "formula".
#' @return
#' Returns a data frame with all the changes applied.
#' @export
#' @examples
#' data<-knn_preprocess(Species ~ ., iris, c("step_center", "step_scale", "step_range"), "Numeric", "Predictor")
#' data<-knn_preprocess(Species ~ ., iris, c("step_dummy"), "Categorical", "Outcome")


knn_preprocess<-function(formula, data, methods = is.character(), apply_on = c("numeric", "categorical"),
                         role = c("predictor","outcome")){

        methods<-match.arg(methods,
                           ls("package:recipes"),
                           several.ok = TRUE)

        apply_on<-match.arg(apply_on)

        role<-match.arg(role)

        recipes<-recipe(formula, data = data)

        step_functions<-map(methods, match.fun)

        data<-map(1:length(step_functions), function(x){

                prep<-recipes %>% step_functions[[x]](has_role(role), has_type(apply_on)) %>% prep()

                bake(prep, data)

        }) %>% .[[length(step_functions)]] %>% as.data.frame()

        return(data)
}
