#' A fit file to create the feature set for test data
#'
#' Takes in the details of features created for train dataset and re-creates it for the test dataset
#' @param dataframe Test dataset
#' @param fcreated_poss A fcreated_poss object returned from fcreate() when returnPossibilities is set to TRUE
#' @param selected_cols A list object returned from fselect()
#' @return A dataframe with created feature based on selected cols provided
#' @export
#' @usage fcreateFit(dataframe, fcreated_poss, selected_cols)
fcreateFit <- function(dataframe, fcreated_poss, selected_cols)
{
  fcreated_poss$names <- paste0(gsub("df[[:punct:]]"," ",as.character(fcreated_poss$term)))
  fcreated_poss$names <- paste0(gsub("`","",as.character(fcreated_poss$names)))
  Selectedpossiblities<-fcreated_poss[fcreated_poss$names %in% selected_cols,]
  term<-as.character(Selectedpossiblities$term)
  fselected_test<-do.call(cbind,pbmclapply(term, getvar, df=dataframe, mc.cores = 1))
  return(fselected_test)
}
