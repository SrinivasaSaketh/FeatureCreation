#' Performs all possible operations on a dataframe, taking 1 or 2 columns at a time
#'
#' Takes in a data frame and performs all possible operations on the columns of a dataframe taking 2 columns at a time. The basic opeartions performed on the columns are as follows:
#' \itemize{
#' \item Addition
#' \item Multiplication
#' \item Division
#' \item Percentile
#' \item Cumulative Sum
#' \item Cumulative Mean
#' \item Sin
#' \item Cosine
#' \item Tan
#' \item Square
#' \item Cube
#' }
#' @param dataframe Any data frame for which the above mentioned operations are to be generated.
#' @param returnPossibilities Flag indicating whether to return list of possibilities or not. Defaulted to FALSE.
#' @return A data frame with all the operations performed when returnPossibilities = FALSE. Return a list of all possibilities otherwise.
#' @export
#' @usage opps(dataframe, returnPossibilities)
opps<-function(dataframe, returnPossibilities = FALSE)
{
  # require(pbmcapply)
  # require(dplyr)
  combi<-data.frame(combinations(dataframe))
  #include symbolic Opps in list below
  oppsbasic<-c("+", "*","/")
  possiblities<-cbind(expand.grid(combi[,1], oppsbasic),Var3=combi[,2])
  possiblities$term<-paste0("df$`",possiblities$Var1,"` ", possiblities$Var2, " ","df$`",possiblities$Var3,"`")
  opps_func <- c("percent_rank", "cumsum", "cummean", "sin", "cos", "tan") #, "cummin", "cummax")
  possibilities_func <- expand.grid(colnames(dataframe), opps_func)
  possibilities_func$Var3 <- NA
  possibilities_func$term <- paste0(possibilities_func$Var2, "(", "df$`", possibilities_func$Var1, "`) ")

  opps_func_def <- c("square", "cube")
  possibilities_func_def <- expand.grid(colnames(dataframe), opps_func_def)
  possibilities_func_def$Var3 <- NA
  possibilities_func_def$term <- paste0(possibilities_func_def$Var2, "(", "df$`", possibilities_func_def$Var1, "`) ")


  possiblities <- rbind(possiblities, possibilities_func,possibilities_func_def)

  if (returnPossibilities == TRUE)
    return(possiblities)
  #Create more combinations using formula in possibilities and include it in term - List must   be unique
  term<-possiblities$term

  output<-do.call(cbind,pbmclapply(term, getvar, df=dataframe, mc.cores = 4))
  output_filtered <- data.table(output)[, !grepl("V[0-9]+", names(output)), with=F]

  return(data.table(output_filtered))

}

#' Features are created by iteratively performing all possible operations on a dataframe based on the depth value, taking 1 or 2 columns at a time
#'
#' Feature creation happens at a depth level. If depth = 1: operations are performed on the raw data provided by the user, If depth = 2: opeartions are performed on the raw data along with the features created at depth 0 adn so on..
#' @param dataframe A dataframe on which features are to be created
#' @param depth depth value
#' @param returnPossibilities Flag indicating whether to return list of possibilities or not. Defaulted to FALSE.
#' @return When returnPossibilities=TRUE, returns the set of all possible features that can be created for a given dataset. Returns the dataframe with features created otherwise.
#' @export
#' @usage opps_gen_iter(dataframe, depth, returnPossibilities)
opps_gen_iter <- function(dataframe, depth, returnPossibilities = FALSE)
{
  for (i in 1:depth)
  {
    inter <- opps(dataframe)
    poss_inter <- opps(dataframe, returnPossibilities = TRUE)
    ifelse(i == 1, possibilities <- poss_inter, possibilities <- rbind(possibilities, poss_inter))
    dataframe <- cbind(dataframe, inter)
    dataframe <- dataframe[, unique(colnames(dataframe))]
    ifelse(returnPossibilities == TRUE, return(possibilities), return(dataframe))
  }

}

#' A consolidated function to create features for a given dataset.
#'
#' This is a consolidated function to create features for a given dataset at any given depth. Check opps() and opps_gen_iter() functions for more details.
#' @param data A dataframe on which features are to be created
#' @param DV Dependent variable in the dataframe in order to eliminate that before creating features
#' @param depth depth value
#' @param returnPossibilities Flag indicating whether to return list of possibilities or not. Defaulted to FALSE.
#' @return When returnPossibilities=TRUE, returns the set of all possible features that can be created for a given dataset. Returns the dataframe with features created otherwise.
#' @export
#' @usage fcreate(data, DV, depth, returnPossibilities)
fcreate <- function(data, DV, depth=2, returnPossibilities = FALSE)
{
  df<-data.frame(data)
  dataframe<-df
  # library(data.table)
  # identifier<-df[,c(DV)]
  df<-df[,!colnames(df) %in% c(DV)]
  q <- opps_gen_iter(df,depth, returnPossibilities)
  if (returnPossibilities == TRUE)
    return (q)
  output<-data.table(cbind(dataframe, data.table(q)[,!colnames(q) %in% colnames(df), with=F]))
  return(output)
}
