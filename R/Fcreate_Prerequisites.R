#' Generate all combinations of the columns, taken 2 at a time
#'
#' Takes in a data frame and generates all combinations of the columns of a dataframe taking 2 columns at a time.
#' @param df Any data frame for which the combination of columns are to be generated.
#' @return An array of all the possible 2 set combinations to all the column names
#' @export
#' @usage combinations(df)
combinations<-function(df)
{
  df_cols<-colnames(df)
  combi<-t(combn(df_cols,2))
  paste("No of Variable Combinations found: ",nrow(combi))
  return(combi)
}

#' Square of a number
#'
#' Takes in a numeric value and generates square of that.
#' @param x A numeric value which needs to be squared
#' @return A numeric value representing the square of the given number
#' @export
#' @usage square(x)
square <- function(x)
{
  return (x^2)
}

#' CUbe of a number
#'
#' Takes in a numeric value and generates cube of that.
#' @param x A numeric value which needs to be cubed
#' @return A numeric value representing the cube of the given number
#' @export
#' @usage cube(x)
cube <- function(x)
{
  return (x^3)
}

#' Creates a column by implementing the operation performed in 'term'
#'
#' Takes in a data frame and an operation to be performed on columns of the dataframe and return a data vector after implementing the given operation
#' @param df A data frame with columns given in term parameter
#' @param term A string which has the operation to be performed uisng two columns of a given data frame
#' @return A data vector derived by the given operation.
#' @export
#' @usage getvar(df, term)
#' @example
#' a <- c(1,2,3,4,5)
#' b <- c(5,4,3,2,1)
#' df <- data.frame(a,b)
#' term <- "df$a + df$b"
#' getvar(df, term)
getvar<-function(df,term)
{
  # require(caret)
  out<-as.data.table(eval(parse(text=term)))
  colnames(out)<-paste0(gsub("df[[:punct:]]"," ",as.character(term)))
  colnames(out)<-paste0(gsub("`","",as.character(colnames(out))))
  ### Condition check
  dummy<-ifelse((nearZeroVar(out, saveMetrics = TRUE)$nzv == FALSE & nearZeroVar(out, saveMetrics = TRUE)$zeroVar == FALSE),
                ifelse(mis_func(out) == FALSE, out <- out, out <- NA), out <- NA)
  return(out)
}

#' Checks the null values (NA, blanks and Infinite values) in a gven data vector
#'
#' Makes a check on the given data vector if it has a minimum 70% data without NULL values
#' @param data_col Any data vector in which the missing data is to be checked
#' @return A boolean value returning TRUE if more than 30% of the data is null and FALSE otherwise
#' @export
#' @usage mis_func(data_col)
#' @example
#' misfunc(c(1,2,Inf, NA, NA))
mis_func <- function(data_col)
{
  data_col<-unlist(data_col)
  mis_count <- sum((is.infinite(data_col)) | (is.na(data_col)) | data_col == "")
  return (ifelse(mis_count/length(data_col) > 0.30, TRUE, FALSE))
}


