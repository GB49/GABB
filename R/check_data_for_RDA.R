#' check_data_for_RDA function
#'
#' This function check if the factor variables used for RDA fit the required conditions.
#' Results are grouped and conclusions go / no go are displayed in the console and saved in a table.
#' This function is mainly derived from the explanations of Maxime Herve
#' \url{https://www.maximeherve.com/r-et-statistiques}
#'
#'
#' @param data_quant Data frame of numeric values, generally transformed and scalled
#' @param initial_data Initial data frame, including both numeric and factor columns
#' @param factor_names Character vector of considered factor variables/columns : c("Variable1", "Variable2")
#'
#' @return Outputs are saved in a data frame and conclusions are displayed in console.
#' @export
#'
#' @examples
#'
#' check_data_for_RDA(mtcars[,1:7], mtcars, c("gear","carb"))
#'

check_data_for_RDA <- function(data_quant, initial_data, factor_names){

  saving_table <- data.frame(1)
  saving_table[1,1:6] <- 1
  names(saving_table) <- c("Df", "Sum Sq", "Mean Sq","F value", "Pr(>F)", "RDA_factor")

  colnumber <- which(names(initial_data)%in%factor_names)

    for (i in colnumber){

    initial_data[,i] <- factor(initial_data[,i])

    nb_modalities <- length(unique(initial_data[,i]))

    if(nb_modalities > 1){
      rda_check <- stats::anova(vegan::betadisper(stats::dist(data_quant), initial_data[,i]))

      factor_considered <- names(initial_data[i])
      factor_considered2 <- paste("Analysis of Variance Table for factor: ", factor_considered, sep="")

      attr(rda_check, "heading") <- paste(factor_considered2,"Response: Distances", sep="\n")

      print(rda_check)

      if(rda_check[1,5] < 0.05){

        message(paste("Variance-covariance matrix of ", factor_considered, " modalities are NOT homogeneous ! (p.val < 0.05)", sep=""))

      }else if(rda_check[1,5] >= 0.05){

        message(paste("Variance-covariance matrix of ", factor_considered, " modalities are homogeneous (p.val >= 0.05)", sep=""))

      }else{NULL}

      rda_check$RDA_factor <- factor_considered
      saving_table <- rbind(saving_table, rda_check)

    }else if(nb_modalities <= 1){

      message(paste("Factor: ", factor_considered ," => only one modality", sep=""))
      message(paste("Test for homogeneity only applicable to two or more groups", sep=""))

    }else{NULL}

  }

  saving_table <- saving_table[-c(1),]
  saving_table[,c(2,3,4)] <- round(saving_table[,c(2,3,4)],2)
  saving_table[,c(5)] <- round(saving_table[,c(5)],3)

  names(saving_table) <- c("Df", "Sum Sq", "Mean Sq","F value", "Pr(>F)", "RDA_factor")
  saving_table <<- data.frame(saving_table)


  conclusion_rda_check <- saving_table[saving_table[,5]<0.05 & is.na(saving_table[,5])==F,]

  if(nrow(conclusion_rda_check)==0){

    message("All variance-covariance matrix of factors modalities are considered homogeneous (pval >= 0.05) => Ok for RDA")

  }else if (nrow(conclusion_rda_check)>0){

    message("For at least one factor, variance-covariance matrix of modalities are NOT considered homogeneous (pval < 0.05) => NOT ok for RDA")
    message("Try to solve the problem through: data transformation, balancing dataset, removing outlier, removing involved factor")

  }else{NULL}

}

