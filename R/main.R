#' Dynamic Naive Bayes Classifier
#'
#' @param data
#' @param extra_info
#' @param control_variable
#'
#' @return
#' @export
#'
#' @examples
dynamic_naive_bayes<- function(data, extra_info, control_variable){

  control_c <- data.frame(control_c = 0)

  extra_info[,ncol(extra_info)+1] <- control_c

  for(i in 1:nrow(extra_info)){
    model <- naive_bayes(data[,control_variable]~., data)

    predict_me <- predict(model, extra_info[i,-ncol(extra_info)])

    extra_info[i,ncol(extra_info)] <- predict_me

    data[nrow(data)+1,] <- extra_info

  }


  data


}
