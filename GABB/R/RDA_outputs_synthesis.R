
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
#'
#' library(vegan)
#' my.RDA <- vegan::rda(mtcars[,1:7]~vs+am+gear, data=mtcars)
#' RDA_outputs_synthesis(RDA = my.RDA, MVAsynth = TRUE, MVAanova = FALSE, RDATable = TRUE)
#'
#'
#'
#'

RDA_outputs_synthesis <- function(RDA, MVAsynth, MVAanova, RDATable){

  if(MVAsynth==TRUE){
    message("Calling MVA.synth() function")
    print(RVAideMemoire::MVA.synt(RDA))

  }

  if(MVAanova==TRUE){
    message("calling function MVA.anova(RDA)")
    print(RVAideMemoire::MVA.anova(RDA))

  }

  if(RDATable==TRUE){

    message("Calculation of variance % associated with each RDA factor, considering unconstrained total variance")
    Table_RDA <- as.data.frame(RVAideMemoire::MVA.anova(RDA))
    Table_RDA$Unconstrained.var.percent <- round((100*Table_RDA$Variance)/(sum(Table_RDA$Variance)),2)
    Table_RDA$Sign.p.val <- NA

    for (i in 1:(nrow(Table_RDA)-1)){

      if (Table_RDA[i,4] <= 0.001){Table_RDA[i,6] <- "***"}
      else if(Table_RDA[i,4] < 0.01 & Table_RDA[i,4] > 0.001){Table_RDA[i,6] <- "**"}
      else if(Table_RDA[i,4]<0.05 & Table_RDA[i,4] >= 0.01){Table_RDA[i,6] <- "*"}
      else if(Table_RDA[i,4]>0.05){Table_RDA[i,6] <- "ns"}
      else{NULL}


    }
    names(Table_RDA)[3] <- "F.val"
    Table_RDA$Variance <- round(Table_RDA$Variance,2)
    Table_RDA$F.val <- round(Table_RDA$F.val,2)

    anovaattr <- RVAideMemoire::MVA.anova(RDA)
    anovaattr <- attr(anovaattr, "heading")
    message(anovaattr)
    print(Table_RDA)

    Table_RDA <<- Table_RDA

  }
}
