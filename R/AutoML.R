#' Automatic Machine Learning
#'
#' The Automatic Machine Learning (AutoML) function automates the supervised machine learning model training process. The current version of AutoML trains and cross-validates a Random Forest, an Extremely-Randomized Forest, a random grid of Gradient Boosting Machines (GBMs), a random grid of Deep Neural Nets, and then trains a Stacked Ensemble using all of the models by using h2o package.
#' @param data A dataframe to be fed into Auto ML. Should contain the dependent variable as "dvcol" in the dataset
#' @return A list with AutoML leaderboard, Variable importance plots, Variable imporatnace measures, parameters and test accuracy for all the models generated
#' @export
#' @usage auto_ml_mydata(data)
auto_ml_mydata <- function(data)
{
  require(caret)
  library(mlbench)
  library(h2o)
  library(rlist)
  library(xgboost)
  library(BestTransform)
  h2o.init()
  h2o.no_progress()
  original_cols <- colnames(data)
  data<-data.frame(data)
  cols_match <- data.frame(original_cols, colnames(data))
  set.seed(1)
  dist=data_distribution(data, "dvcol")
  dvtype<-as.character(dist[dist$names=="dvcol",3])


  #Encode Catagorical data as Factors & Convert to H2o Objects
  if(dvtype=="Continous"){
    data[,"dvcol"]<-data[,"dvcol"]
    y <- "dvcol"
    x <- setdiff(names(data), c(y))
    data<-as.h2o(data)
    splits<-h2o.splitFrame(data, ratios = 0.8, seed = 1)
    df_tr<-splits[[1]]
    df_te<-splits[[2]]
    test_dup <- as.data.frame(df_te)
    colnames(test_dup)[ncol(test_dup)] <- "obs"
  } else
  {
    data[,"dvcol"]<-as.factor(data[,"dvcol"])
    y <- "dvcol"
    x <- setdiff(names(data), c(y))
    lvl<-unique(data$dvcol)
    data<-as.h2o(data)
    splits<-h2o.splitFrame(data, ratios = 0.8, seed = 1)
    df_tr<-splits[[1]]
    df_te<-splits[[2]]
    test_dup <- as.data.frame(df_te)
    colnames(test_dup)[ncol(test_dup)] <- "obs"
  }

  model <- h2o.automl(y = y, x = x,
                      training_frame = df_tr,
                      max_models = 10,
                      seed = 1)

  # plots<-list()
  varimps<-list()
  parameters<-list()
  test_perf<-list()

  leaderboard_df<-as.data.frame(model@leaderboard)
  model_ids <- as.data.frame(model@leaderboard$model_id)[,1]
  model_ids <- model_ids[!grepl("Ensemble", model_ids)]
  for(i in model_ids){
    # Get the "All Models" Stacked Ensemble model
    se <- h2o.getModel(grep(i, model_ids, value = TRUE)[1])

    # Get the Stacked Ensemble metalearner model
    varimp_model<-as.data.frame(h2o.varimp(se))
    # h2o.varimp_plot(se)

    # varimp_plot<-recordPlot(h2o.varimp_plot(se))
    param_df<-as.data.frame(unlist(se@allparameters))
    param_df$parameter<-rownames(param_df)
    colnames(param_df)<-c("Values","Metrics")

    parameters<-list.append(parameters,param_df)
    varimps<-list.append(varimps,varimp_model)
    # plots<-list.append(plots,varimp_plot)

    predicted <- as.data.frame(h2o.predict(object=se, newdata=df_te))
    test_dup$pred<-predicted$predict


    if(dvtype=="Continous"){
      test_val<-postResample(pred = test_dup$pred, obs = test_dup$obs)
    }else{
      if(length(lvl) > 2){test_val<- multiClassSummary(as.data.frame(test_dup), lev = levels(test_dup$obs))}
      else{test_val<- q<- prSummary(test_dup, lev = levels(test_dup$obs))}
    }

    test_perf<-list.append(test_perf,test_val)

  }

  return (list(cols_match = cols_match, leaderboard = leaderboard_df,varimps = varimps,parameters = parameters,test_perf = test_perf))


}
plot_H2o_models <- function (vi, model_name, cols_match, num_of_features = NULL)
{
  if (is.null(num_of_features)) {
    feature_count = length(vi$variable)
    num_of_features = ifelse(feature_count <= 10, length(vi$variable),
                             10)
  } else if ((num_of_features != round(num_of_features)) || (num_of_features <= 0))
    stop("num_of_features must be an integer greater than 0")
  title = paste("Variable Importance: ", model_type = strsplit(model_name, "_")[[1]][1],
                sep = "")
  vi <-left_join(vi, cols_match, by = c("variable" = "colnames.data."))
  ylabels = vi$original_cols
  ymargin <- max(strwidth(ylabels, "inch") + 0.4, na.rm = TRUE)
  par(mai = c(1.02, ymargin, 0.82, 0.42))
  if (num_of_features == 1) {
    barplot(rev(head(vi$scaled_importance, n = num_of_features)),
            names.arg = rev(head(vi$original_cols, n = num_of_features)),
            width = 0.2, space = 1, horiz = TRUE, las = 2, ylim = c(0,
                                                                    2), xlim = c(0, 1), axes = TRUE, col = "#1F77B4",
            main = title)
  }
  else if (num_of_features > 1) {
    barplot(rev(head(vi$scaled_importance, n = num_of_features)),
            names.arg = rev(head(vi$original_cols, n = num_of_features)),
            space = 1, las = 2, horiz = TRUE, col = "#1F77B4",
            main = title)
  }
}


data <- iris
data$dvcol <- data$Species
data$Species <- NULL
colnames(data) <- c("(Sepal.Length)", "(Sepal.Width)",  "(Petal.Length)", "(Petal.Width)",  "dvcol")

res <- auto_ml_mydata(data)
plot_H2o_models()
