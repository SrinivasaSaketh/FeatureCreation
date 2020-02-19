#' Calculate variance inflation factor (VIF) from the result and lm() and use that to eliminate the columns with high VIF
#'
#' Of all the features created using fcreate(), it is possible that some of the features can be multi-collinear with each other. So it is a good practice to eliminate such columns before passing the features to a machine learning model. This function eliminates all columns that have VIF that is greater than the threshold provided.
#' @param in_frame A dataframe with feature to be tested for multi-collinearity
#' @param thresh A cut-off for VIF value. Defaulted to 10
#' @param trace A flag which when true, prints results after each iteration. Defaulted to TRUE
#' @return A data frame with all the multi-collinear features eliminated
#' @export
#' @usage vif_func(in_frame, thresh, trace)
vif_func<-function(in_frame,thresh=10,trace=T)
{
  # library(fmsb)
  if(any(!'data.frame' %in% class(in_frame))) in_frame<-data.frame(in_frame)
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- colnames(in_frame)
  for(val in var_names){
    # print(val)
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame))))
  }
  vif_init_df <- data.frame(vif_init)
  vif_init <- as.matrix(vif_init[vif_init_df$X2 != Inf,])
  vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
  in_frame <- data.table(in_frame)[,colnames(in_frame) %in% vif_init[,1], with=F]
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    # return(var_names)
  }else{
    in_dat<-in_frame
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    i=1
    while(vif_max >= thresh){
      # print(i)
      vif_vals<-NULL
      var_names <- names(in_dat)

      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
      vif_max<-as.numeric(vif_vals[max_row,2])
      if(vif_max<thresh) {break}
      if(trace==T){ #print output of each iteration
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        cat(length(colnames(in_dat)) - 1, " variables left to check for multi-collinearity\n")
        flush.console()
      }
      in_dat<-data.table(in_dat)[,!names(in_dat) %in% vif_vals[max_row,1], with=F]
      i = i + 1
    }

    # return(names(in_dat))
    return (in_dat)
  }
}

#' Eliminate all columns in a dataframe that have high multi-collinearity
#'
#' Of all the features created using fcreate(), it is possible that some of the features can be multi-collinear with each other. So it is a good practice to eliminate such columns before passing the features to a machine learning model. This function eliminates all columns that have VIF that is greater than the threshold provided.
#' @param fcreated A dataframe with features to be tested for multi-collinearity and eliminate features with high multi-collinearity
#' @return Returns a list of two objects
#' \itemize{
#' \item A dataframe with all the multi-collinear columns removed
#' \item A fit file indicating the list of columns that are selected
#' }
#' @export
#' @usage fselect(fcreated)
fselect <- function(fcreated)
{
  # require(data.table)
  w<-data.table(Columnnames=colnames(fcreated))
  w$newcolumnnames<-row.names(w)
  w$newcolumnnames<-paste0("V",w$newcolumnnames)
  colnames(fcreated)<-w$newcolumnnames

  fcreated[is.na(fcreated)] <- 0
  is.na(fcreated)<-sapply(fcreated, is.infinite)
  fcreated[is.na(fcreated)]<-0
  is.na(fcreated)<-sapply(fcreated, is.nan)
  fcreated[is.na(fcreated)]<-0

  f_selected <- vif_func(in_frame = fcreated, thresh=5, trace=TRUE)
  if (is.null(f_selected))
    return(list(fselected_data = NULL, selected_cols = NULL))
  colnames(f_selected) <- as.character(data.table(w)[newcolumnnames %in% colnames(f_selected),Columnnames])

  selected_cols <- colnames(f_selected)
  return(list(fselected_data = f_selected, selected_cols = selected_cols))
}

