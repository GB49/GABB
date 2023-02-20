
#' RDA_outputs_synthesis function
#'
#' This function facilitate the use and outputs synthesis of RDA analysis performed by the vegan and RVAideMemoire packages.
#' This function is mainly derived from the explanations of Maxime Herve
#' \url{https://www.maximeherve.com/r-et-statistiques}
#'
#' @param RDA The RDA object : RDA <- vegan::rda(mtcars_quant~gear+carb,data=mtcars)
#' @param MVAsynth TRUE or FALSE. TRUE : displaying RVAideMemoire::MVA.synth(RDA) outputs.
#' @param MVAanova TRUE or FALSE. TRUE : displaying RVAideMemoire::MVA.anova(RDA) outputs.
#' @param RDATable TRUE or FALSE. TRUE : calculate and display the variance percentage of considered factor / total unconstrained variance.
#'
#' @return Results can be displayed in the console. Outputs are saved in a data frame
#' @export
#'
#' @examples
#' \dontrun{
#' RDA_outputs_synthesis(RDA, TRUE, TRUE, TRUE)
#'
#' }
#'
#'

RDA_outputs_synthesis <- function(RDA, MVAsynth, MVAanova, RDATable){

  if(MVAsynth==T){
    cat("calling function MVA.synth(RDA)")
    cat("\n")
    print(RVAideMemoire::MVA.synt(RDA))
    cat("\n")
    cat("\n")
  }

  if(MVAanova==T){
    cat("calling function MVA.anova(RDA)")
    cat("\n")
    print(RVAideMemoire::MVA.anova(RDA))
    cat("\n")
    cat("\n")
  }

  if(RDATable==T){

    cat("Calculation of variance % associated with each RDA factor, considering unconstrained total variance")
    cat("\n")

    Table_RDA <- as.data.frame(RVAideMemoire::MVA.anova(RDA))
    Table_RDA$Unconstrained_variance_percent <- round((100*Table_RDA$Variance)/(sum(Table_RDA$Variance)),2)
    Table_RDA$Sign_pval <- NA

    for (i in 1:(nrow(Table_RDA)-1)){

      if (Table_RDA[i,4] <= 0.001){Table_RDA[i,6] <- "***"}
      else if(Table_RDA[i,4] < 0.01 & Table_RDA[i,4] > 0.001){Table_RDA[i,6] <- "**"}
      else if(Table_RDA[i,4]<0.05 & Table_RDA[i,4] >= 0.01){Table_RDA[i,6] <- "*"}
      else if(Table_RDA[i,4]>0.05){Table_RDA[i,6] <- "ns"}
      else{NULL}


    }

    Table_RDA$Variance <- round(Table_RDA$Variance,2)
    Table_RDA$F <- round(Table_RDA$F,2)

    anovaattr <- RVAideMemoire::MVA.anova(RDA)
    anovaattr <- attr(anovaattr, "heading")

    cat(anovaattr)
    cat("\n")
    cat("\n")

    print(Table_RDA)

    Table_RDA <<- Table_RDA

  }
}
