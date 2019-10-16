#' @title knn Preprocess
#' @description This function applies step functions of recipes package
#' @author Alberto Almuiña
#' @param formula Formula that will be evaluated in algorithm (for example: Species ~ .)
#' @param data Dataframe that will be modified
#' @param methods Steps functions of the recipes package to be applied. For example( c("step_center", "step_scale", "step_range"))
#' @param not_apply_on "nominal" or "numeric". The column type in which step functions will not be applied. 
#' @param role "predictor" or "outcome". Outcome is the target variable and "Predictor" represent all the rest in "formula".
#' @return
#' Returns a data frame with all the changes applied.
#' @export
#' @examples
#' data<-knn_preprocess(Species ~ ., iris, c("step_center", "step_scale", "step_range"), "numeric", "predictor")
#' data<-knn_preprocess(Species ~ ., iris, c("step_dummy"), "nominal", "outcome")


knn_preprocess<-function(formula, data, methods = is.character(), not_apply_on = c("numeric", "nominal"),
                         role = c("predictor","outcome"), ...){
        
        methods<-match.arg(methods, 
                           ls("package:recipes"),
                           several.ok = TRUE)
        
        not_apply_on<-match.arg(not_apply_on)
        
        role<-match.arg(role)
        
        recipes<-recipe(formula, data = data)

        step_functions<-map(methods, match.fun)

        data<-map(1:length(step_functions), function(x){

                prep<-recipes %>% step_functions[[x]](has_role(role), -has_type(not_apply_on)) %>% prep()

                bake(prep, data)

        }) %>% .[[length(step_functions)]] %>% as.data.frame()
        
        return(data)
}
