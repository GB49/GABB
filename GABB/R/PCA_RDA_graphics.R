#' PCA_RDA_graphics function
#' This function facilitate the user creation of PCA (! from FactoMineR package !) enhanced graphics with multiple options.
#' Individuals and variables graphics are possible. With or without variable cor, cos, contrib.. correlation to dimensions.
#' RDA outputs are displayable as table under graphics.
#'
#' @param complete.data.set Original complete data set used for the PCA, with quantitative and qualitative/factor columns
#' @param factor.names Character vector of considered factor variables of PCA
#' @param sample.column Numeric or name (character) of the individual sample column. Default is data frame row names.
#' @param PCA.object The PCA object, built from package FactoMineR. PCA.object <- FactoMineR::PCA(mtcars_quant, scale.unit = T, ncp = 5, graph = F)
#' @param Var.quanti.supp Character vector of column name of supplementary quantitative variables for PCA.
#' @param Display.quanti.supp TRUE or FALSE. Display supplementary quantitative variable on PCA var and Heat map graphs.
#' @param Dim.a Numeric value (1 ; 2 ...) of the first PCA dimension selected for graphic outputs.
#' @param Dim.b Numeric value (1 ; 2 ...) of the first PCA dimension selected for graphic outputs.
#' @param Multi.dim.combination TRUE or FALSE. TRUE = display 10 plots of PCA ind and variables dims combination (1,2 ; 1,3...)
#' @param Barycenter TRUE or FALSE. TRUE : Calculate and Display the barycenter of individuals for Barycenter.Ellipse.Fac1 and/or .2 and/or .3.
#' @param Segments TRUE or FALSE. TRUE : Display the linking segments between individuals and barycenters.
#' @param Barycenter.min.size Numeric. Minimum size of barycenter point projections. Ignore if Barycenter = FALSE
#' @param Ind.min.size Numeric. Minimum size of individuals point projections.
#' @param Segment.line.type Numeric. Type of segment lines (see ggplot2 line type). Ignore if Segments = FALSE
#' @param Segment.line.size Numeric. Minimum size of segment lines. Ignore if Segments = FALSE
#' @param Segment.line.col Character. Set the color of segments. Default = azure4
#' @param Ellipse.IC.95 TRUE or FALSE. TRUE : Calculate and Display the Ellipse (95% mean confidence intervals) of individuals for Barycenter.Ellipse.Fac1 and/or .2 and/or .3.
#' @param Barycenter.Ellipse.Fac1 Character. Name of 1st factor/data frame column for Barycenter / Ellipses calculation.
#' @param Barycenter.Ellipse.Fac2 Character. Name of 2nd factor/data frame column for Barycenter / Ellipses calculation.
#' @param Barycenter.Ellipse.Fac3 Character. Name of 3rd factor/data frame column for Barycenter / Ellipses calculation.
#' @param factor.colors Character. Name of the factor/column considered for individuals colors.
#' @param color.palette Vector of characters of desired colors.
#' @param factor.shapes Character. Name of the factor/column considered for individual and barycenter shapes.
#' @param factor.sizes Character. Name of the factor/column considered for individual and barycenter colors.
#' @param factor.col.border.ellipse Character. Name of the factor/column considered for ellipse border colors.
#' @param Barycenter.factor.col Character. Name of the factor/column considered for barycenter colors.
#' @param Barycenter.factor.size Character. Name of the factor/column considered for barycenter size.
#' @param Barycenter.factor.shape Character. Name of the factor/column considered for barycenter shape.
#' @param ellipse.line.type Numeric. R line type for ellipse borders.
#' @param Var.circle TRUE or FALSE. TRUE = Display the PCA variable circle projection.
#' @param Var.circle.size Numeric. Value for increasing the size of Var.circle graphic.
#' @param Var.label.size Numeric. Value for increasing the size of Var.circle graphic labels.
#' @param Var.label.repel TRUE or FALSE. For PCA variables graphic, force variable labels to repel.
#' @param Var.selected Character vector of selected variables for the PCA plot. Default = all variables.
#' @param col.arrow.var.PCA Character. Set the color of arrows for PCA variable plot. Default = gray20
#' @param col.arrow.var.supp.PCA Character. Set the color of arrows and text for PCA supplementary variable plot. Default = cadetblue
#' @param col.text.var.PCA Character. Set the color of text for PCA variable plot. Default = gray20
#' @param col.circle.var.PCA Character. Set the color of the PCA variable circle. Default = gray20
#' @param Biplot.PCA TRUE or FALSE. TRUE = Biplot of PCA individuals and variables graphics. Default is set to FALSE.
#' @param width.PCA.ind.graph Numeric. Width ratio for PCA individuals graphic.
#' @param width.PCA.var.graph Numeric. Width ratio for PCA variables graphic.
#' @param width.heat.map.graph Numeric. Width ratio for Heat map variables graphic.
#' @param Heat.map.graph TRUE or FALSE. TRUE = Display the heat map of variable X parameter correlation to dimension.
#' @param Type.heat.map.graph Character. Define the type of heat map to display : "square" or "circle". Default = "square".
#' @param var.parameter.heat.map Character. Parameter selected for the heat map correlation of Variable parameter to dimensions. values : "cor", "cos2", "coor","contrib". Default = "cor".
#' @param Dims.heat.map Numeric. Numeric vector c(1,2) of dimensions considered for the variable parameter correlation.
#' @param Top.var.heat.map.Dim.a Numeric. Number of variables to plot in heat maps and PCA variable projection for Dim a. Default = all.
#' @param Top.var.heat.map.Dim.b Numeric. Number of variables to plot in heat maps and PCA variable projection for Dim b. Default = all.
#' @param Display.cell.values.heat.map TRUE or FALSE. TRUE = Display the rounded value of correlations within heat map cells.
#' @param width.cell.heat.map Numeric. Width  for Heat map cells. Default set to default pheat.map.
#' @param height.cell.heat.map Numeric. Width  for Heat map cells. Default set to default pheat.map.
#' @param Cluster.col.heat.map TRUE or FALSE. TRUE = cluster heat.map columns / dimensions.
#' @param Cluster.row.heat.map TRUE or FALSE. TRUE = cluster heat.map rows / quantitative variables.
#' @param RDA.object The RDA object, built from package vegan. RDA.object <- vegan::rda(mtcars_quant, scale.unit = T, ncp = 5, graph = F)
#' @param RDA.table.graph TRUE or FALSE. TRUE = Display the RDA outputs table under PCA graphics.
#' @param Size.RDA.table.graph Numeric. Set the ratio of RDA table graphic size. Default is set to 7.
#' @param RDA.table.graph.height Numeric. Set the ratio of RDA table graphic height. Default is set to 1.
#' @param Get.generated.data.frame TRUE or FALSE. TRUE = save the generated data frames for graphic constructions. Default = FALSE
#'
#' @return Several graphics
#' @export
#'
#' @examples
#'
#' library(FactoMineR)
#' my.PCA <- FactoMineR::PCA(mtcars[,1:7], scale.unit = FALSE, ncp = 5, graph = FALSE)
#' PCA_RDA_graphics(complete.data.set = mtcars, factor.names = c("vs", "gear"), PCA.object = my.PCA)
#'
#'

PCA_RDA_graphics <- function(complete.data.set, factor.names, sample.column,
                             PCA.object, Var.quanti.supp, Display.quanti.supp, Dim.a, Dim.b, Barycenter, Segments,
                             Barycenter.min.size, Ind.min.size, Segment.line.type, Segment.line.size,Segment.line.col,
                             Ellipse.IC.95, Barycenter.Ellipse.Fac1, Barycenter.Ellipse.Fac2, Barycenter.Ellipse.Fac3,
                             factor.colors, color.palette, factor.shapes, factor.sizes,
                             Barycenter.factor.col, Barycenter.factor.size, Barycenter.factor.shape,
                             factor.col.border.ellipse, ellipse.line.type,
                             Var.circle, Var.circle.size, Var.selected, Var.label.size, Var.label.repel,
                             col.arrow.var.PCA, col.text.var.PCA, col.arrow.var.supp.PCA, col.circle.var.PCA, Biplot.PCA,
                             width.PCA.ind.graph, width.PCA.var.graph, width.heat.map.graph,
                             Heat.map.graph,Type.heat.map.graph, var.parameter.heat.map, Dims.heat.map, Display.cell.values.heat.map,
                             Top.var.heat.map.Dim.a, Top.var.heat.map.Dim.b,Multi.dim.combination,
                             width.cell.heat.map, height.cell.heat.map,Cluster.col.heat.map, Cluster.row.heat.map,
                             RDA.object, RDA.table.graph, RDA.table.graph.height,Size.RDA.table.graph, Get.generated.data.frame){





  #Define default parameters
  if(missing(Get.generated.data.frame)){

    Get.generated.data.frame <- FALSE

  }

    if(missing(Barycenter.factor.col)){

    Barycenter.factor.col <- "void"

  }

  if(missing(Barycenter.factor.size)){

    Barycenter.factor.size <- "void"

  }

  if(missing(Barycenter.factor.shape)){

    Barycenter.factor.shape <- "void"

  }


  if(missing(col.text.var.PCA)){

    col.text.var.PCA <- "gray20"

  }

  if(missing(Segment.line.col)){

    Segment.line.col <- "azure4"

  }

  if(missing(col.arrow.var.PCA)){

    col.arrow.var.PCA <- "gray20"

  }

  if(missing(col.arrow.var.supp.PCA)){

    col.arrow.var.supp.PCA <- "cadetblue"

  }

  if(missing(col.circle.var.PCA)){

    col.circle.var.PCA <- "gray20"

  }

  if(missing(Multi.dim.combination)){

    Multi.dim.combination <- FALSE


  }

  if(missing(Var.label.repel)){

    Var.label.repel <- FALSE


  }

  if(missing(Var.quanti.supp)){

    Var.quanti.supp <- FALSE

  }

  if(missing(Display.quanti.supp)){

    Display.quanti.supp <- FALSE

  }


  if(missing(Size.RDA.table.graph)){

    Size.RDA.table.graph <- 7

  }

  if(missing(width.cell.heat.map)){

    width.cell.heat.map <- FALSE

  }

  if(missing(height.cell.heat.map)){

    height.cell.heat.map <- FALSE

  }

  if(missing(RDA.table.graph)){

    RDA.table.graph <- FALSE

  }

  if(missing(Type.heat.map.graph)){

    Type.heat.map.graph <- "square"

  }


  if(missing(Cluster.row.heat.map)){

    Cluster.row.heat.map <- FALSE

  }

  if(missing(Cluster.col.heat.map)){

    Cluster.col.heat.map <- FALSE

  }

  if(missing(Display.cell.values.heat.map)){

    Display.cell.values.heat.map <- TRUE

  }

  if(missing(var.parameter.heat.map)){

    var.parameter.heat.map <- "cor"

  }

  if(missing(Ind.min.size)){

    Ind.min.size <- FALSE

  }

  if(missing(factor.sizes)){

    factor.sizes <- FALSE

  }
    if(missing(factor.shapes)){

    factor.shapes <- FALSE

  }

    if(missing(factor.colors)){

    factor.colors <- FALSE

  }

  if(missing(color.palette)){

    color.palette <- FALSE

  }

  if(missing(Var.selected)){

    Var.selected <- FALSE

  }

  if(missing(factor.col.border.ellipse)){

    factor.col.border.ellipse <- FALSE

  }

  if(missing(ellipse.line.type)){

    ellipse.line.type <- FALSE

  }

  if(missing(Barycenter)){

    Barycenter <- FALSE

  }

  if(missing(Barycenter.min.size)){

    Barycenter.min.size <- FALSE

  }

  if(missing(Segments)){

    Segments <- FALSE

  }

  if(missing(Ellipse.IC.95)){

    Ellipse.IC.95 <- FALSE

  }

  if(missing(RDA.object)){

    is.RDA.object <- FALSE

  }else if(missing(RDA.object)==FALSE){

    is.RDA.object <- TRUE

  }

  if(missing(Heat.map.graph)){

    Heat.map.graph <- FALSE

  }


  if(missing(Dim.a)){
    Dim.a <- 1
  }

  if(missing(Dim.b)){
    Dim.b <- 2
  }

  #Empty data frame for missing parameters
  barycentre <- data.frame(0,0,0,0)
  vec1 <- c(Dim.a, Dim.b)
  vec11 <- paste("Dim.",vec1,sep="")
  vec11b <- paste(vec11,"_b",sep="")
  names(barycentre) <- c(vec11,vec11b)

  barycentre_ind <- barycentre
  barycentre_ind$facteur_IC95.1 <- 0
  barycentre_ind$facteur_IC95.2 <- 0


  Table_RDA <- data.frame(0)

  #Get columns number of factors
  colnumber <- which(names(complete.data.set)%in%factor.names)

  #Extract coords for individuals and variables
  data_ind_ACP <- cbind(data.frame(cbind(PCA.object$ind$coord[,c(1:5)])), complete.data.set[,c(colnumber)])


  nbnames.post.dims <- as.numeric(length(names(data_ind_ACP)))
  names(data_ind_ACP)[6:nbnames.post.dims] <- factor.names

  data_var_ACP <- cbind(data.frame(cbind(PCA.object$var$coord[,c(1:5)])))

  if(is.character(Var.quanti.supp)==TRUE){

    rownumbers <- which(rownames(PCA.object[["quanti.sup"]][["coord"]])%in%Var.quanti.supp)
    quantisup <- as.data.frame(PCA.object[["quanti.sup"]][["coord"]])
    quantisup <- as.data.frame(quantisup[rownumbers,])

    data_var_ACP <- rbind(data_var_ACP, quantisup)

  }


  #Define considered "factor columns" as factors

  colnumber2 <- which(names(data_ind_ACP)%in%factor.names)
  for (i in unique(colnumber2)){

    data_ind_ACP[,i] <- as.factor(data_ind_ACP[,i])

  }

  #Sample names : individuals

  if(missing(sample.column)==T){

    data_ind_ACP$Samples <- row.names(complete.data.set)

  }else if(is.numeric(sample.column)==T){

    data_ind_ACP$Samples <- complete.data.set[,sample.column]

  }else if(is.character(sample.column)==T){

    colnumbersample <- which(names(complete.data.set)%in%sample.column)
    data_ind_ACP$Samples <- complete.data.set[,colnumbersample]

  }else{NULL}

  #Variables names
  data_var_ACP$Var.names <- rownames(data_var_ACP)
  Var.names_column <- which(names(data_var_ACP)%in%"Var.names")


  #Barycenter calculation
  Dima <- paste("Dim.",Dim.a,sep="")
  Dimb <- paste("Dim.",Dim.b,sep="")

  Dima2 <- which(names(data_ind_ACP)%in%Dima)
  Dimb2 <- which(names(data_ind_ACP)%in%Dimb)

  if(Barycenter==FALSE & missing(Barycenter.Ellipse.Fac1)==TRUE & missing(Barycenter.Ellipse.Fac2)==TRUE & missing(Barycenter.Ellipse.Fac3)==TRUE){

    void <- 1

  }else if(Barycenter==TRUE & missing(Barycenter.Ellipse.Fac1)==TRUE & missing(Barycenter.Ellipse.Fac2)==TRUE & missing(Barycenter.Ellipse.Fac3)==TRUE){

    void <- 1
    warning("Barycenter must have a factor defined")

  }else if(missing(Barycenter.Ellipse.Fac2)==TRUE & missing(Barycenter.Ellipse.Fac3)==TRUE){

    F1 <- which(names(data_ind_ACP)%in%Barycenter.Ellipse.Fac1)
    min_comb <- as.numeric(min(summary(data_ind_ACP[,F1])))
    max_comb <- as.numeric(max(summary(data_ind_ACP[,F1])))

      barycentre1<-stats::aggregate(data_ind_ACP[,Dima2]~data_ind_ACP[,F1], data = data_ind_ACP, FUN = mean)
      names(barycentre1)[ncol(barycentre1)] <- Dima

      barycentre2<-stats::aggregate(data_ind_ACP[,Dimb2]~data_ind_ACP[,F1], data = data_ind_ACP, FUN = mean)
      names(barycentre2)[ncol(barycentre2)] <- Dimb

      barycentre<-dplyr::inner_join(barycentre1, barycentre2)
      names(barycentre)[1] <- Barycenter.Ellipse.Fac1
      names(barycentre)[c((ncol(barycentre)-1):ncol(barycentre))]<-paste(names(barycentre)[c((ncol(barycentre)-1):ncol(barycentre))], "b", sep="_") #Numero des colonnes
      barycentre_ind<-dplyr::left_join(data_ind_ACP, barycentre)

      #Calcul de l'intervalle de confiance 95%
      IC95_sd <- stats::aggregate(data_ind_ACP[,Dima2]~data_ind_ACP[,F1], data = data_ind_ACP, FUN = stats::sd) ; names(IC95_sd)[2] <- "sd.1"
      names(IC95_sd)[1] <- Barycenter.Ellipse.Fac1

      IC95_n <-stats::aggregate(data_ind_ACP[,Dima2]~data_ind_ACP[,F1], data = data_ind_ACP, FUN = length) ; names(IC95_n)[2] <- "nb.1"
      names(IC95_n)[1] <- Barycenter.Ellipse.Fac1

      IC95_1 <- dplyr::inner_join(IC95_sd,IC95_n)

      IC95_sd <-stats::aggregate(data_ind_ACP[,Dimb2]~data_ind_ACP[,F1], data = data_ind_ACP, FUN = stats::sd) ; names(IC95_sd)[2] <- "sd.2"
      names(IC95_sd)[1] <- Barycenter.Ellipse.Fac1

      IC95_n <-stats::aggregate(data_ind_ACP[,Dimb2]~data_ind_ACP[,F1], data = data_ind_ACP, FUN = length) ; names(IC95_n)[2] <- "nb.2"
      names(IC95_n)[1] <- Barycenter.Ellipse.Fac1

      IC95_2 <- dplyr::inner_join(IC95_sd,IC95_n)

      barycentre_ind<-dplyr::inner_join(barycentre_ind, IC95_1)
      barycentre_ind<-dplyr::inner_join(barycentre_ind, IC95_2)

      barycentre_ind$facteur_IC95.1 <- ((1.96*barycentre_ind$sd.1)/sqrt(barycentre_ind$nb.1))
      barycentre_ind$facteur_IC95.2 <- ((1.96*barycentre_ind$sd.2)/sqrt(barycentre_ind$nb.2))

      dataellipse <- barycentre_ind[is.na(barycentre_ind$sd.1),]

      if(Ellipse.IC.95==TRUE & nrow(dataellipse)>0){

        message(paste(as.numeric(nrow(dataellipse)), " factor or factor combination without ellipse because only 1 individual / modality", sep=""))

      }

      barycentre_ind[is.na(barycentre_ind)] <- 0

  }else if(missing(Barycenter.Ellipse.Fac2)==FALSE & missing(Barycenter.Ellipse.Fac3)==TRUE){

    F1 <- which(names(data_ind_ACP)%in%Barycenter.Ellipse.Fac1)
    F2 <- which(names(data_ind_ACP)%in%Barycenter.Ellipse.Fac2)

    data_ind_ACP$nb_comb <- as.factor(paste(data_ind_ACP[,F1], data_ind_ACP[,F2]))

    min_comb <- as.numeric(min(summary(data_ind_ACP[,ncol(data_ind_ACP)])))
    max_comb <- as.numeric(max(summary(data_ind_ACP[,ncol(data_ind_ACP)])))

      barycentre1<-stats::aggregate(data_ind_ACP[,Dima2]~data_ind_ACP[,F1]+data_ind_ACP[,F2], data = data_ind_ACP, FUN = mean)
      names(barycentre1)[ncol(barycentre1)] <- Dima

      barycentre2<-stats::aggregate(data_ind_ACP[,Dimb2]~data_ind_ACP[,F1]+data_ind_ACP[,F2], data = data_ind_ACP, FUN = mean)
      names(barycentre2)[ncol(barycentre2)] <- Dimb

      barycentre<-dplyr::inner_join(barycentre1, barycentre2)
      names(barycentre)[1] <- Barycenter.Ellipse.Fac1
      names(barycentre)[2] <- Barycenter.Ellipse.Fac2

      names(barycentre)[c((ncol(barycentre)-1):ncol(barycentre))]<-paste(names(barycentre)[c((ncol(barycentre)-1):ncol(barycentre))], "b", sep="_") #Numero des colonnes
      barycentre_ind<-dplyr::left_join(data_ind_ACP, barycentre)

      #Calcul de l'intervalle de confiance 95%
      IC95_sd <-stats::aggregate(data_ind_ACP[,Dima2]~data_ind_ACP[,F1]+data_ind_ACP[,F2], data = data_ind_ACP, FUN = stats::sd) ; names(IC95_sd)[3] <- "sd.1"
      names(IC95_sd)[1] <- Barycenter.Ellipse.Fac1
      names(IC95_sd)[2] <- Barycenter.Ellipse.Fac2

      IC95_n <-stats::aggregate(data_ind_ACP[,Dima2]~data_ind_ACP[,F1]+data_ind_ACP[,F2], data = data_ind_ACP, FUN = length) ; names(IC95_n)[3] <- "nb.1"
      names(IC95_n)[1] <- Barycenter.Ellipse.Fac1
      names(IC95_n)[2] <- Barycenter.Ellipse.Fac2

      IC95_1 <- dplyr::inner_join(IC95_sd,IC95_n)

      IC95_sd <-stats::aggregate(data_ind_ACP[,Dimb2]~data_ind_ACP[,F1]+data_ind_ACP[,F2], data = data_ind_ACP, FUN = stats::sd) ; names(IC95_sd)[3] <- "sd.2"
      names(IC95_sd)[1] <- Barycenter.Ellipse.Fac1
      names(IC95_sd)[2] <- Barycenter.Ellipse.Fac2

      IC95_n <-stats::aggregate(data_ind_ACP[,Dimb2]~data_ind_ACP[,F1]+data_ind_ACP[,F2], data = data_ind_ACP, FUN = length) ; names(IC95_n)[3] <- "nb.2"
      names(IC95_n)[1] <- Barycenter.Ellipse.Fac1
      names(IC95_n)[2] <- Barycenter.Ellipse.Fac2
      IC95_2 <- dplyr::inner_join(IC95_sd,IC95_n)

      barycentre_ind<-dplyr::inner_join(barycentre_ind, IC95_1)
      barycentre_ind<-dplyr::inner_join(barycentre_ind, IC95_2)

      barycentre_ind$facteur_IC95.1 <- ((1.96*barycentre_ind$sd.1)/sqrt(barycentre_ind$nb.1))
      barycentre_ind$facteur_IC95.2 <- ((1.96*barycentre_ind$sd.2)/sqrt(barycentre_ind$nb.2))

      dataellipse <- barycentre_ind[is.na(barycentre_ind$sd.1),]

      if(Ellipse.IC.95==TRUE & nrow(dataellipse)>0){

        message(paste(as.numeric(nrow(dataellipse)), " factor or factor combination without ellipse because only 1 individual / modality", sep=""))

      }

      barycentre_ind[is.na(barycentre_ind)] <- 0


  }else if(missing(Barycenter.Ellipse.Fac2)==FALSE & missing(Barycenter.Ellipse.Fac3)==FALSE){

    F1 <- which(names(data_ind_ACP)%in%Barycenter.Ellipse.Fac1)
    F2 <- which(names(data_ind_ACP)%in%Barycenter.Ellipse.Fac2)
    F3 <- which(names(data_ind_ACP)%in%Barycenter.Ellipse.Fac3)

    data_ind_ACP$nb_comb <- as.factor(paste(data_ind_ACP[,F1], data_ind_ACP[,F2], data_ind_ACP[,F3]))

    min_comb <- as.numeric(min(summary(data_ind_ACP[,ncol(data_ind_ACP)])))
    max_comb <- as.numeric(max(summary(data_ind_ACP[,ncol(data_ind_ACP)])))

      barycentre1<-stats::aggregate(data_ind_ACP[,Dima2]~data_ind_ACP[,F1]+data_ind_ACP[,F2]+data_ind_ACP[,F3], data = data_ind_ACP, FUN = mean)
      names(barycentre1)[ncol(barycentre1)] <- Dima

      barycentre2<-stats::aggregate(data_ind_ACP[,Dimb2]~data_ind_ACP[,F1]+data_ind_ACP[,F2]+data_ind_ACP[,F3], data = data_ind_ACP, FUN = mean)
      names(barycentre2)[ncol(barycentre2)] <- Dimb

      barycentre<-dplyr::inner_join(barycentre1, barycentre2)
      names(barycentre)[1] <- Barycenter.Ellipse.Fac1
      names(barycentre)[2] <- Barycenter.Ellipse.Fac2
      names(barycentre)[3] <- Barycenter.Ellipse.Fac3

      names(barycentre)[c((ncol(barycentre)-1):ncol(barycentre))]<-paste(names(barycentre)[c((ncol(barycentre)-1):ncol(barycentre))], "b", sep="_") #Numero des colonnes
      barycentre_ind<-dplyr::left_join(data_ind_ACP, barycentre)

      #Calcul de l'intervalle de confiance 95%
      IC95_sd <-stats::aggregate(data_ind_ACP[,Dima2]~data_ind_ACP[,F1]+data_ind_ACP[,F2]+data_ind_ACP[,F3], data = data_ind_ACP, FUN = stats::sd) ; names(IC95_sd)[4] <- "sd.1"
      names(IC95_sd)[1] <- Barycenter.Ellipse.Fac1
      names(IC95_sd)[2] <- Barycenter.Ellipse.Fac2
      names(IC95_sd)[3] <- Barycenter.Ellipse.Fac3

      IC95_n <-stats::aggregate(data_ind_ACP[,Dima2]~data_ind_ACP[,F1]+data_ind_ACP[,F2]+data_ind_ACP[,F3], data = data_ind_ACP, FUN = length) ; names(IC95_n)[4] <- "nb.1"
      names(IC95_n)[1] <- Barycenter.Ellipse.Fac1
      names(IC95_n)[2] <- Barycenter.Ellipse.Fac2
      names(IC95_n)[3] <- Barycenter.Ellipse.Fac3

      IC95_1 <- dplyr::inner_join(IC95_sd,IC95_n)

      IC95_sd <-stats::aggregate(data_ind_ACP[,Dimb2]~data_ind_ACP[,F1]+data_ind_ACP[,F2]+data_ind_ACP[,F3], data = data_ind_ACP, FUN = stats::sd) ; names(IC95_sd)[4] <- "sd.2"
      names(IC95_sd)[1] <- Barycenter.Ellipse.Fac1
      names(IC95_sd)[2] <- Barycenter.Ellipse.Fac2
      names(IC95_sd)[3] <- Barycenter.Ellipse.Fac3

      IC95_n <-stats::aggregate(data_ind_ACP[,Dimb2]~data_ind_ACP[,F1]+data_ind_ACP[,F2]+data_ind_ACP[,F3], data = data_ind_ACP, FUN = length) ; names(IC95_n)[4] <- "nb.2"
      names(IC95_n)[1] <- Barycenter.Ellipse.Fac1
      names(IC95_n)[2] <- Barycenter.Ellipse.Fac2
      names(IC95_n)[3] <- Barycenter.Ellipse.Fac3

      IC95_2 <- dplyr::inner_join(IC95_sd,IC95_n)

      barycentre_ind<-dplyr::inner_join(barycentre_ind, IC95_1)
      barycentre_ind<-dplyr::inner_join(barycentre_ind, IC95_2)

      barycentre_ind$facteur_IC95.1 <- ((1.96*barycentre_ind$sd.1)/sqrt(barycentre_ind$nb.1))
      barycentre_ind$facteur_IC95.2 <- ((1.96*barycentre_ind$sd.2)/sqrt(barycentre_ind$nb.2))

      dataellipse <- barycentre_ind[is.na(barycentre_ind$sd.1),]

      if(Ellipse.IC.95==TRUE & nrow(dataellipse)>0){

        message(paste(as.numeric(nrow(dataellipse)), " factor or factor combination without ellipse because only 1 individual / modality", sep=""))

      }

      barycentre_ind[is.na(barycentre_ind)] <- 0

  }else{NULL}

  #Arguments for plot
  #Individuals
  data_ind_ACP$IPZ <- 2

  if(Ind.min.size==FALSE){

    Ind.min.size <- 2

  }

  if (factor.sizes==FALSE & Ind.min.size!=FALSE){

    data_ind_ACP$IPZ <- Ind.min.size
    factor.sizes.forced <- 1

  }else if (factor.sizes!=FALSE & Ind.min.size!=FALSE){

    factor.sizes.forced <- factor.sizes
    IPZf <- which(names(data_ind_ACP)%in%factor.sizes)
    data_ipz <- data.frame(unique(data_ind_ACP[,IPZf]))
    names(data_ipz) <- factor.sizes
    data_ipz$IPZ2 <- seq(Ind.min.size, (Ind.min.size+nrow(data_ipz)-1), by=1)

    data_ind_ACP <- dplyr::left_join(data_ind_ACP, data_ipz)
    data_ind_ACP$IPZ <- data_ind_ACP$IPZ2

  }


  #Barycenter
  barycentre$BCZ <- 3
  barycentre$BCZfill <- "white"
  barycentre$BCZcol <- "black"


  if(Barycenter==FALSE){

    barycentre$BCZfill <- NA
    barycentre$BCZcol <- NA
    barycentre$BCZ <- 0


  }

  if(Barycenter.min.size!=FALSE){

    barycentre$BCZ <- Barycenter.min.size

  }

  if(Barycenter!=FALSE & factor.colors==FALSE){

    colBar <- which(names(barycentre)%in%Barycenter.Ellipse.Fac1)
    barycentre$BCZfill <- barycentre[,colBar]


  }

  if(Barycenter!=FALSE & factor.colors!=FALSE){

    colBar <- which(names(barycentre)%in%factor.colors)
    barycentre$BCZfill <- barycentre[,colBar]


  }



  #Ellipses

  barycentre_ind$EFill <- "white"
  barycentre_ind$Ecol <- "black"
  barycentre_ind$CBE <- "black"

  ELT <- 1

  if (Ellipse.IC.95==FALSE){

    barycentre_ind$EFill <- NA
    barycentre_ind$Ecol <- NA
    barycentre_ind$CBE <- NA

  }else if (Ellipse.IC.95==TRUE & factor.colors==FALSE){

    colBar <- which(names(barycentre_ind)%in%Barycenter.Ellipse.Fac1)

    barycentre_ind$EFill <- barycentre_ind[,colBar]

  }else if (Ellipse.IC.95==TRUE & factor.colors!=FALSE){

    colBar <- which(names(barycentre_ind)%in%factor.colors)

    barycentre_ind$EFill <- barycentre_ind[,colBar]

  }

  if(factor.col.border.ellipse!=FALSE){

    colBar <- which(names(barycentre_ind)%in%factor.col.border.ellipse)
    barycentre_ind$CBE <- barycentre_ind[,colBar]

  }

  if(ellipse.line.type!=FALSE){

    ELT <- ellipse.line.type

  }

  #Segments

  if (missing(Segment.line.type)==TRUE){

    SLT <- 1

  }else if (missing(Segment.line.type)==FALSE){

    SLT <- Segment.line.type

  }

  if (missing(Segment.line.size)==TRUE){

    SLS <- 0

  }else if (missing(Segment.line.size)==FALSE){

    SLS <- Segment.line.size

  }

  SLC <- Segment.line.col

  if(Segments==FALSE | missing(Segments)==TRUE){

    SLC = NA

  }

  #Default variable circle and label sizes
  if(missing(Var.label.size)==TRUE){
    kz <- 4
  }else if (missing(Var.label.size)==FALSE){
    kz <- Var.label.size
  }

  if(missing(Var.circle.size)==TRUE){
    k <- 1
  }else if (missing(Var.circle.size)==FALSE){
    k <- Var.circle.size
  }


  colnumber_dima3 <- Dima
  Dima3 <- which(names(barycentre_ind)%in%colnumber_dima3)

  colnumber_dima4 <- paste(Dima, "_b", sep="")
  Dima4 <- which(names(barycentre_ind)%in%colnumber_dima4)

  colnumber_dima5 <- paste(Dima, "_b", sep="")
  Dima5 <- which(names(barycentre)%in%colnumber_dima4)

  colnumber_dimb3 <- Dimb
  Dimb3 <- which(names(barycentre_ind)%in%colnumber_dimb3)

  colnumber_dimb4 <- paste(Dimb, "_b", sep="")
  Dimb4 <- which(names(barycentre_ind)%in%colnumber_dimb4)

  colnumber_dimb5 <- paste(Dimb, "_b", sep="")
  Dimb5 <- which(names(barycentre)%in%colnumber_dimb5)


  VD1 <- which(names(data_var_ACP)%in%Dima)
  VD2 <- which(names(data_var_ACP)%in%Dimb)



  #IND colors and shape default parameters
  if(factor.colors==FALSE){

    data_ind_ACP$f.colors <- "one.col"


  }else if(factor.colors!=FALSE){

    FC10 <- which(names(data_ind_ACP)%in%factor.colors)
    data_ind_ACP$f.colors <- data_ind_ACP[,FC10]

  }

  FC <- which(names(data_ind_ACP)%in%"f.colors")

  if(factor.shapes==FALSE){

    data_ind_ACP$f.shapes <- "one.shape"

  }else if(factor.shapes!=FALSE){

    FS10 <- which(names(data_ind_ACP)%in%factor.shapes)
    data_ind_ACP$f.shapes <- data_ind_ACP[,FS10]

  }

  FS <- which(names(data_ind_ACP)%in%"f.shapes")

  vector_shapes <- c(21,22,24,25,23)
  vector_shapes <- vector_shapes[1:length(unique(data_ind_ACP[,FS]))]

  d1 <- paste("Dim1 (",round(PCA.object$eig[1,2],1),"%)", sep="")
  d2 <- paste("Dim2 (",round(PCA.object$eig[2,2],1),"%)", sep="")
  d3 <- paste("Dim3 (",round(PCA.object$eig[3,2],1),"%)", sep="")
  d4 <- paste("Dim4 (",round(PCA.object$eig[4,2],1),"%)", sep="")
  d5 <- paste("Dim5 (",round(PCA.object$eig[5,2],1),"%)", sep="")

  da <- ifelse(Dim.a==1, d1,
               ifelse(Dim.a==2, d2,
                      ifelse(Dim.a==3, d3,
                             ifelse(Dim.a==4, d4,
                                    ifelse(Dim.a==5, d5, NULL)))))

  db <- ifelse(Dim.b==1, d1,
               ifelse(Dim.b==2, d2,
                      ifelse(Dim.b==3, d3,
                             ifelse(Dim.b==4, d4,
                                    ifelse(Dim.b==5, d5, NULL)))))




  #Defaut widths
  if(missing(width.PCA.ind.graph)==TRUE){

    WidthIG <- 1

  }else if(missing(width.PCA.ind.graph)==FALSE){

  WidthIG <- width.PCA.ind.graph

  }

  if(missing(width.PCA.var.graph)==TRUE){

    WidthVG <- 1

  }else if(missing(width.PCA.var.graph)==FALSE){

    WidthVG <- width.PCA.var.graph

  }

  if(missing(width.heat.map.graph)==TRUE){

    WidthHM <- 0.3

  }else if(missing(width.heat.map.graph)==FALSE){

    WidthHM <- width.heat.map.graph

  }


  #legends

  factor.colors.legend <- NA
  factor.shapes.legend <- NA
  factor.sizes.legend <- NA



  if(factor.colors!=FALSE){
    factor.colors.legend <- factor.colors
  }
  if(factor.shapes!=FALSE){
    factor.shapes.legend <- factor.shapes
  }
  if(factor.sizes!=FALSE){
    factor.sizes.legend <- factor.sizes.forced
  }

  #RDA table output as graph

  if(is.RDA.object==FALSE){

    RDA.table.graph <- FALSE

  }else if(is.RDA.object==TRUE){

  Table_RDA <- as.data.frame(RVAideMemoire::MVA.anova(RDA.object))
  Table_RDA$Unconstrained.var.percent <- round((100*Table_RDA$Variance)/(sum(Table_RDA$Variance)),2)
  Table_RDA$Sign.p.val <- NA

  Table_RDA <<- Table_RDA

  for (i in 1:(nrow(Table_RDA)-1)){

    if (Table_RDA[i,4] <= 0.001){Table_RDA[i,6] <- "***"}
    else if(Table_RDA[i,4] < 0.01 & Table_RDA[i,4] > 0.001){Table_RDA[i,6] <- "**"}
    else if(Table_RDA[i,4]<0.05 & Table_RDA[i,4] >= 0.01){Table_RDA[i,6] <- "*"}
    else if(Table_RDA[i,4]>0.05){Table_RDA[i,6] <- "ns"}
    else{NULL}


  }

  names(Table_RDA)[3] <- "F.val"
  names(Table_RDA)[5] <- paste("Unconstr.", "Var(%)", sep="\n")
  names(Table_RDA)[6] <- paste("Sign", "p.val", sep="\n")

  Table_RDA$Variance <- round(Table_RDA$Variance,2)
  names(Table_RDA)[2] <- "Var"

  Table_RDA$F.val <- round(Table_RDA$F.val,2)

  Table_RDA$Factor <- rownames(Table_RDA)
  Table_RDA$Factor <- gsub("Residual", "residuals", Table_RDA$Factor)

  text.subtitle <- paste("Model: ", as.character(RDA.object$call)[2], sep="")

  tab <- ggpubr::ggtexttable(Table_RDA, theme = ggpubr::ttheme("light", base_size = Size.RDA.table.graph), rows=NULL)
  tab <- ggpubr::tab_add_title(tab, text = "Redundancy analysis", face="bold")
  tab <- ggpubr::table_cell_font(tab, column = c(3,4,6),row = (nrow(Table_RDA)+2),
                                 face="italic", color="darkgrey", size=8)

  tab <- ggpubr::tab_add_border(tab)
  tab <- ggpubr::tab_add_footnote(tab, text=text.subtitle, face="italic", size=8)
  Table_RDA_graph <- tab
  Table_RDA_graph <<- Table_RDA_graph

  }


  if(missing(RDA.table.graph)==T | RDA.table.graph==F){

    HeightRDA <- 0

  }else if(missing(RDA.table.graph)==F & RDA.table.graph==T){

    if(missing(RDA.table.graph.height)==T){

      HeightRDA <- 1

    }else if(missing(RDA.table.graph.height)==F){

      HeightRDA <- RDA.table.graph.height

    }else{NULL}

  }else{NULL}



  #Heatmap for variable cos2, contrib or cor to dims

  if(missing(Heat.map.graph)==T | Heat.map.graph==F){

    WidthHM <- 0

  }else if(missing(Heat.map.graph)==F & Heat.map.graph==T){

    if(is.character(Var.quanti.supp)==TRUE & Display.quanti.supp==TRUE){

          if(var.parameter.heat.map=="coord"){

            matrixHM0 <- as.data.frame(PCA.object[[5]][1])
            names(matrixHM0) <- gsub("coord.","",names(matrixHM0))
            gng0 <- "ok"

          }else if(var.parameter.heat.map=="cor"){

            matrixHM0 <- as.data.frame(PCA.object[[5]][2])
            names(matrixHM0) <- gsub("cor.","",names(matrixHM0))
            gng0 <- "ok"

          }else if(var.parameter.heat.map=="cos2"){

            matrixHM0 <- as.data.frame(PCA.object[[5]][3])
            names(matrixHM0) <- gsub("cos2.","",names(matrixHM0))
            gng0 <- "ok"

          }else if(var.parameter.heat.map=="contrib"){

            matrixHM0 <- as.data.frame(PCA.object[[2]][4])
            matrixHM0 <- as.data.frame(matrixHM0[1,])
            gng0 <- "not.ok"
            message("Variable contribution to dims: no display of supplementary quantitative variables")

          }else if (var.parameter.heat.map != "coord" | var.parameter.heat.map != "cor" |var.parameter.heat.map != "cos2" |var.parameter.heat.map != "contrib"){

            gng <- "nook"

          }else {NULL}

    }


    if(var.parameter.heat.map=="coord"){

      matrixHM <- as.data.frame(PCA.object[[2]][1])
      names(matrixHM) <- gsub("coord.","",names(matrixHM))
      gng <- "ok"
      textvar <- "coordinates"

    }else if(var.parameter.heat.map=="cor"){

      matrixHM <- as.data.frame(PCA.object[[2]][2])
      names(matrixHM) <- gsub("cor.","",names(matrixHM))
      gng <- "ok"
      textvar <- "correlations"

    }else if(var.parameter.heat.map=="cos2"){

      matrixHM <- as.data.frame(PCA.object[[2]][3])
      names(matrixHM) <- gsub("cos2.","",names(matrixHM))
      gng <- "ok"
      textvar <- "cos2"

    }else if(var.parameter.heat.map=="contrib"){

      matrixHM <- as.data.frame(PCA.object[[2]][4])
      names(matrixHM) <- gsub("contrib.","",names(matrixHM))
      gng <- "ok"
      textvar <- "contributions"

    }else if (var.parameter.heat.map != "coord" | var.parameter.heat.map != "cor" |var.parameter.heat.map != "cos2" |var.parameter.heat.map != "contrib"){

      gng <- "nook"

    }else {NULL}

    if(gng=="nook"){

      warning("Please choose valid PCA variable parameter (coord, cor, cos2 or contrib")

    }else if (gng=="ok"){

      if(missing(Dims.heat.map)==TRUE){

        Dims.heat.map <- c(1,2)

      }else if(missing(Dims.heat.map)==FALSE){

        Dims.heat.map <- Dims.heat.map

      }

      #Filtering top variables for heat.maps

      if(missing(Top.var.heat.map.Dim.a)){

        Top.var.heat.map.Dim.a <- nrow(PCA.object$var$coord)

      }

      if(missing(Top.var.heat.map.Dim.b)){

        Top.var.heat.map.Dim.b <- nrow(PCA.object$var$coord)

      }


      if(is.character(Var.quanti.supp)==TRUE & Display.quanti.supp==TRUE){

      matrixHM$Var.names <- rownames(matrixHM)
      matrixHM0$Var.names <- rownames(matrixHM0)

      matrixHM <- dplyr::full_join(matrixHM,matrixHM0)
      rownames(matrixHM) <- matrixHM$Var.names
      matrixHM <- matrixHM[,-c(ncol(matrixHM))]

      }

      hm.a <- dplyr::arrange(matrixHM, dplyr::desc(abs(matrixHM[,Dim.a])))
      hm.b <- dplyr::arrange(matrixHM, dplyr::desc(abs(matrixHM[,Dim.b])))

      hm.a <- hm.a[c(1:Top.var.heat.map.Dim.a),]
      hm.b <- hm.b[c(1:Top.var.heat.map.Dim.b),]

      matrixHM <- rbind(hm.a,hm.b)
      matrixHM <- matrixHM[!duplicated(matrixHM),]

      matrixHM <<- matrixHM


      #Getting number for hm cell

      DHM <- paste("Dim.",Dims.heat.map,sep="")
      colnumber_pheatmap <- which(names(matrixHM)%in%DHM)

      text_pheatmap <- matrixHM[,colnumber_pheatmap]

      if( Display.cell.values.heat.map==F){
        colheatmap <- NA

      }else if(Display.cell.values.heat.map==TRUE){
        colheatmap <- "black"

      }

      if(missing(Cluster.col.heat.map)==T | Cluster.col.heat.map==F){

        CC <- F

      }else if(missing(Cluster.col.heat.map)==F & Cluster.col.heat.map==T){

        CC <- T

      }

      if(missing(Cluster.row.heat.map)==T | Cluster.row.heat.map==F){

        CR <- F

      }else if(missing(Cluster.row.heat.map)==F & Cluster.row.heat.map==T){

        CR <- T

      }





      Pheatmap_var_graph <- pheatmap::pheatmap(matrixHM[,colnumber_pheatmap],
                                               display_numbers = round(text_pheatmap,2),
                                               number_color = colheatmap,
                                               cluster_rows = CR,
                                               cluster_cols = CC,
                                               angle_col = 0)

      if(width.cell.heat.map!=FALSE & height.cell.heat.map==FALSE){

        Pheatmap_var_graph <- pheatmap::pheatmap(matrixHM[,colnumber_pheatmap],
                                                 display_numbers = round(text_pheatmap,2),
                                                 number_color = colheatmap,
                                                 cluster_rows = CR,
                                                 cluster_cols = CC,
                                                 angle_col = 0,
                                                 cellwidth = width.cell.heat.map)


      }else if(width.cell.heat.map==FALSE & height.cell.heat.map!=FALSE){

        Pheatmap_var_graph <- pheatmap::pheatmap(matrixHM[,colnumber_pheatmap],
                                                 display_numbers = round(text_pheatmap,2),
                                                 number_color = colheatmap,
                                                 cluster_rows = CR,
                                                 cluster_cols = CC,
                                                 angle_col = 0,
                                                 cellheight = height.cell.heat.map)


      }else if(width.cell.heat.map!=FALSE & height.cell.heat.map!=FALSE){

        Pheatmap_var_graph <- pheatmap::pheatmap(matrixHM[,colnumber_pheatmap],
                                                 display_numbers = round(text_pheatmap,2),
                                                 number_color = colheatmap,
                                                 cluster_rows = CR,
                                                 cluster_cols = CC,
                                                 angle_col = 0,
                                                 cellwidth = width.cell.heat.map,
                                                 cellheight = height.cell.heat.map)


      }

      Pheatmap_var_graph <- ggplotify::as.ggplot(Pheatmap_var_graph)

      Pheatmap_var_graph <- Pheatmap_var_graph + egg::theme_article()+
        ggplot2::labs(title=paste("Variable ", textvar, " to dims", sep=""))+
        ggplot2::xlab(" ")+
        ggplot2::ylab("Void")+
        ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                       axis.ticks.y = ggplot2::element_blank(),
                       axis.text.y = ggplot2::element_blank(),

                       axis.title.x = ggplot2::element_text(colour=NA),
                       axis.ticks.x = ggplot2::element_line(colour=NA),
                       axis.text.x = ggplot2::element_text(colour=NA))

      Pheatmap_var_graph <<- Pheatmap_var_graph

    }else{NULL}
  }

  if(Heat.map.graph==TRUE & Type.heat.map.graph=="circle"){

  #heatmap with circles
  #dumy void object
  Dims <- "void"
  Val <- "void"

  DVC <- matrixHM
  DVC$Var.names <- rownames(matrixHM)

  DVC$yposition <- c(1:nrow(DVC))

  DVC2 <- tidyr::gather(DVC, key = Dims,value = Val, 1:(ncol(DVC)-2))

  DVC2$xposition <- 0
  xposition_col <-  as.numeric(which(names(DVC2)%in%"xposition"))
  dims_col <-  as.numeric(which(names(DVC2)%in%"Dims"))

  for (i in 2:nrow(DVC2)){

    DVC2[1,xposition_col] <- 1

    if(DVC2[i,dims_col]!=DVC2[(i-1),dims_col]){

      DVC2[i,xposition_col] <- DVC2[(i-1),xposition_col]+1

    }else if(DVC2[i,dims_col]==DVC2[(i-1),dims_col]){

      DVC2[i,xposition_col] <- DVC2[(i-1),xposition_col]

    }else{NULL}


  }


  minxpos <- min(DVC2[,xposition_col])
  maxxpos <- max(DVC2[,xposition_col])

  Circle.dims <- which(names(DVC2)%in%"Dims")
  Circle.Val <- which(names(DVC2)%in%"Val")
  Circle.Var.names <- which(names(DVC2)%in%"Var.names")
  Circle.yposition <- which(names(DVC2)%in%"yposition")


  Circle_heatmap_var_graph <- ggplot2::ggplot()+
    ggplot2::geom_point(data=DVC2 , ggplot2::aes(y=DVC2[,Circle.Var.names], x=DVC2[,Circle.dims],
                                       size=abs(DVC2[,Circle.Val]), fill=DVC2[,Circle.Val]),
                        shape=21)+
    ggplot2::scale_fill_gradient2(low = "blue3",mid = "white" ,high = "brown1",midpoint = 0, limits=c(-1,1))+
    ggplot2::geom_segment(data=DVC2, ggplot2::aes(x=minxpos, xend=maxxpos, y=DVC2[,Circle.yposition], yend=DVC2[,Circle.yposition]),
                          color="grey", linewidth=0.1, linetype=3)+
    ggplot2::theme_light()+ggplot2::theme(panel.grid = ggplot2::element_blank())+
    ggplot2::guides(fill=ggplot2::guide_colourbar("Value"))+
    ggplot2::guides(size=ggplot2::guide_legend("abs(Value)"))+
    ggplot2::ylab("")+
    ggplot2::xlab("")+
    ggplot2::ggtitle(paste("Variable ", textvar, " to dims", sep=""))+
    ggplot2::scale_x_discrete(expand = c(0.06,0.1))

  Circle_heatmap_var_graph <<- Circle_heatmap_var_graph

  }

  #Dummy objects, to be overwrite
  Vcol <- "void"

  #Individuals PCA Plot

  Efill_column <- which(names(barycentre_ind)%in%"EFill")
  Ecol_column <- which(names(barycentre_ind)%in%"Ecol")
  CBE_column <- which(names(barycentre_ind)%in%"CBE")

  facteur_IC95.1_column <- which(names(barycentre_ind)%in%"facteur_IC95.1")
  facteur_IC95.2_column <- which(names(barycentre_ind)%in%"facteur_IC95.2")

  IPZ_column <- which(names(data_ind_ACP)%in%"IPZ")

  bie <- barycentre_ind[!duplicated(barycentre_ind[,facteur_IC95.1_column]),]

  bie_bary <- dplyr::left_join(bie, barycentre)


  BCZ_column <- which(names(bie_bary)%in%"BCZ")
  BCZcol_column <- which(names(bie_bary)%in%"BCZcol")
  BCZfill_column <- which(names(bie_bary)%in%"BCZfill")


  #Barycenter graphic parameter


  if(Barycenter.factor.col!="void"){

    bary_col <- which(names(bie_bary)%in%Barycenter.factor.col)
    bie_bary[,BCZfill_column] <- bie_bary[,bary_col]


  }

  if(Barycenter.factor.size!="void"){

    bary_size <- which(names(bie_bary)%in%Barycenter.factor.size)
    bie_bary[,BCZ_column] <- bie_bary[,bary_size]

    bie_bary[,BCZ_column] <- factor(bie_bary[,BCZ_column])

    bie_bary <- bie_bary[order(bie_bary[,BCZ_column]),]
    vec_size <- rep(as.numeric(unique(barycentre$BCZ)), nrow(bie_bary))

    for(i in 2:nrow(bie_bary)){

      if(bie_bary[i,BCZ_column] == bie_bary[(i-1),BCZ_column]){

        vec_size[i] <- vec_size[i-1]


      }else{vec_size[i] <- vec_size[i-1]+1}


    }

    bie_bary[,BCZ_column] <- as.numeric(vec_size)


  }

  if(Barycenter.factor.shape!="void"){

    bary_shape <- which(names(bie_bary)%in%Barycenter.factor.shape)
    bie_bary$BCShp <- bie_bary[,bary_shape]

    bie_bary$BCShp <- as.factor(bie_bary$BCShp)

    BCShp_column <- which(names(bie_bary)%in%"BCShp")

  }


  colnumber_dima5 <- paste(Dima, "_b", sep="")
  Dima51 <- which(names(bie_bary)%in%colnumber_dima4)

  colnumber_dimb5 <- paste(Dimb, "_b", sep="")
  Dimb51 <- which(names(bie_bary)%in%colnumber_dimb5)




  #Graphics : individuals PCA
  PCA_ind_graphic <- ggplot2::ggplot()+
    ggplot2::geom_segment(ggplot2::aes(x = barycentre_ind[,Dima3], y = barycentre_ind[,Dimb3], xend = barycentre_ind[,Dima4], yend = barycentre_ind[,Dimb4]),
                          linetype=2,linewidth=SLS, data = barycentre_ind, color = SLC)

  if(Ellipse.IC.95==FALSE){

    PCA_ind_graphic <- PCA_ind_graphic + ggforce::geom_ellipse(ggplot2::aes(x0 = bie[,Dima4], y0 = bie[,Dimb4],
                                                                   a = (bie[,facteur_IC95.1_column]), b = (bie[,facteur_IC95.2_column]),
                                                                   angle = 0, color=I(bie[,Ecol_column])),fill=NA, alpha=0.1, data=bie)

  }else if(Ellipse.IC.95==TRUE){


    if(factor.col.border.ellipse!=FALSE){


        PCA_ind_graphic <- PCA_ind_graphic + ggforce::geom_ellipse(ggplot2::aes(x0 = bie[,Dima4], y0 = bie[,Dimb4],
                                                                                a = (bie[,facteur_IC95.1_column]), b = (bie[,facteur_IC95.2_column]),
                                                                                angle = 0,fill=bie[,Efill_column], color=bie[,CBE_column]), alpha=0.1, data=bie, size=0.1, linetype=ELT)


    }else if(factor.col.border.ellipse==FALSE){

    PCA_ind_graphic <- PCA_ind_graphic + ggforce::geom_ellipse(ggplot2::aes(x0 = bie[,Dima4], y0 = bie[,Dimb4],
                                                                             a = (bie[,facteur_IC95.1_column]), b = (bie[,facteur_IC95.2_column]),
                                                                             angle = 0,fill=bie[,Efill_column]), color="azure4", alpha=0.1, data=bie, size=0.1, linetype=ELT)

    }
  }

  if(factor.colors==FALSE){
    data_ind_ACP[,FC] <- "black"
  }


    PCA_ind_graphic <- PCA_ind_graphic + ggplot2::geom_point(ggplot2::aes(x=data_ind_ACP[,Dima2], y=data_ind_ACP[,Dimb2], fill=data_ind_ACP[,FC], shape=data_ind_ACP[,FS], size=data_ind_ACP[,IPZ_column]), data=data_ind_ACP, alpha=1)+
    ggplot2::geom_point(ggplot2::aes(x=data_ind_ACP[,Dima2], y=data_ind_ACP[,Dimb2], fill=data_ind_ACP[,FC], shape=data_ind_ACP[,FS], size=data_ind_ACP[,IPZ_column]), data=data_ind_ACP, fill=NA, colour="black")


    if(Barycenter.factor.shape!="void"){

    PCA_ind_graphic <- PCA_ind_graphic + ggplot2::geom_point(ggplot2::aes(x=bie_bary[,Dima51], y=bie_bary[,Dimb51], fill=bie_bary[,BCZfill_column], size=bie_bary[,BCZ_column], shape=bie_bary[,BCShp_column]),
                                                             colour="black", data = bie_bary,show.legend = F)+
      ggplot2::scale_shape_manual(values=c(21:25))


    }else if(Barycenter.factor.shape=="void"){

    PCA_ind_graphic <- PCA_ind_graphic + ggplot2::geom_point(ggplot2::aes(x=bie_bary[,Dima51], y=bie_bary[,Dimb51], fill=bie_bary[,BCZfill_column], size=bie_bary[,BCZ_column]),
                                                             colour="black", shape=23, data = bie_bary,show.legend = F)+
      ggplot2::scale_shape_manual(values = vector_shapes)

    }

    if(Barycenter.factor.size!="void"){

      PCA_ind_graphic <- PCA_ind_graphic+
      ggplot2::scale_size_identity(name=as.character(Barycenter.factor.size),
                                   breaks=bie_bary[,BCZ_column],
                                   labels=bie_bary[,bary_size], guide="legend")



    }

    if(Barycenter.factor.size!="void" & factor.sizes==FALSE){

      PCA_ind_graphic <- PCA_ind_graphic+ggplot2::labs(fill=factor.colors.legend, shape=factor.shapes.legend)+
        ggplot2::guides(fill=ggplot2::guide_legend(override.aes = list(shape=1)))

    }

    if(Barycenter.factor.size=="void" & factor.sizes==FALSE){

    PCA_ind_graphic <- PCA_ind_graphic+ggplot2::labs(fill=factor.colors.legend, shape=factor.shapes.legend)+
    ggplot2::guides(fill=ggplot2::guide_legend(override.aes = list(shape=1)))+
    ggplot2::scale_size_identity()+ggplot2::guides(size = "none")

    }

    if(factor.sizes!=F){

    PCA_ind_graphic <- PCA_ind_graphic+ggplot2::labs(fill=factor.colors.legend, shape=factor.shapes.legend, size=factor.sizes.legend)+
    ggplot2::guides(fill=ggplot2::guide_legend(override.aes = list(shape=1)))+
    ggplot2::scale_size_identity()

    }

    if(factor.colors==FALSE){

      PCA_ind_graphic <- PCA_ind_graphic+ggplot2::guides(fill = "none")

    }else if(factor.colors!=FALSE){

      PCA_ind_graphic <- PCA_ind_graphic

    }else{NULL}


    if(factor.shapes==FALSE){

      PCA_ind_graphic <- PCA_ind_graphic+ggplot2::guides(shape = "none")

    }else if(factor.shapes!=FALSE){

      PCA_ind_graphic <- PCA_ind_graphic

    }else{NULL}

    PCA_ind_graphic <- PCA_ind_graphic+ ggplot2::ggtitle("PCA : Sample projection")+
    ggplot2::xlab(da)+
    ggplot2::ylab(db)+

    egg::theme_article()+
    ggplot2::theme(strip.background =ggplot2::element_rect(fill="white"))+
    ggplot2::theme(strip.text = ggplot2::element_text(colour = 'black'))

    if(is.character(color.palette)==TRUE){

      PCA_ind_graphic <- PCA_ind_graphic+ggplot2::scale_fill_manual(values=color.palette)

    }

  PCA_ind_graphic <- PCA_ind_graphic+ggplot2::coord_fixed()
  PCA_ind_graphic <<- PCA_ind_graphic

  var.title <- "PCA : Variable projection"

  #Filtering top variables for heat.maps

  if(missing(Top.var.heat.map.Dim.a)){

    Top.var.heat.map.Dim.a <- nrow(data_var_ACP)

  }

  if(missing(Top.var.heat.map.Dim.b)){

    Top.var.heat.map.Dim.b <- nrow(data_var_ACP)

  }

  dva.a <- dplyr::arrange(data_var_ACP, dplyr::desc(abs(data_var_ACP[,Dim.a])))
  dva.b <- dplyr::arrange(data_var_ACP, dplyr::desc(abs(data_var_ACP[,Dim.b])))

  dva.a <- dva.a[c(1:Top.var.heat.map.Dim.a),]
  dva.b <- dva.b[c(1:Top.var.heat.map.Dim.b),]

  data_var_ACP <- rbind(dva.a,dva.b)
  data_var_ACP <- data_var_ACP[!duplicated(data_var_ACP),]

  var.names.pca <- rownames(PCA.object$var$coord)
  data_var_ACP_var.col <- which(rownames(data_var_ACP) %in% var.names.pca)
  data_var_ACP_var <- data_var_ACP[data_var_ACP_var.col,]

  if(is.character(Var.selected)==TRUE){

    data_var_ACP_var <- data_var_ACP_var[data_var_ACP_var$Var.names %in% Var.selected,]
    var.title <- "PCA : Variable subset projection"
  }

  if(Display.quanti.supp==TRUE & is.character(Var.quanti.supp)==TRUE){

    var.quanti.names.pca <- rownames(PCA.object$quanti.sup$coord)
    data_var_ACP_var.col2 <- which(rownames(data_var_ACP) %in% var.quanti.names.pca)
    data_var_ACP_var.quanti <- data_var_ACP[data_var_ACP_var.col2,]
    data_var_ACP_var <- rbind(data_var_ACP_var, data_var_ACP_var.quanti)

    data_var_ACP_var$my.col <- col.arrow.var.PCA
    data_var_ACP_var.col2 <- which(data_var_ACP_var$Var.names %in% var.quanti.names.pca)
    data_var_ACP_var[data_var_ACP_var.col2,7] <- col.arrow.var.supp.PCA

    Vcol <- which(names(data_var_ACP_var) %in% "my.col")

    data_var_ACP_var$my.col.text <- col.text.var.PCA
    Vcol.text <- which(names(data_var_ACP_var) %in% "my.col.text")
    data_var_ACP_var[data_var_ACP_var.col2,8] <- col.arrow.var.supp.PCA


  }else if(Display.quanti.supp==FALSE | is.character(Var.quanti.supp)==FALSE){

    data_var_ACP_var$my.col <- col.arrow.var.PCA
    Vcol <- which(names(data_var_ACP_var) %in% "my.col")

    data_var_ACP_var$my.col.text <- col.text.var.PCA
    Vcol.text <- which(names(data_var_ACP_var) %in% "my.col.text")

  }




  PCA_var_graphic <- ggplot2::ggplot()+
    ggforce::geom_circle(ggplot2::aes(x0 = 0, y0 = 0, r = 1*k), color = col.circle.var.PCA, linetype = 2, alpha = 0.5)+
    ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = data_var_ACP_var[,VD1]*k, yend = data_var_ACP_var[,VD2]*k,
                          colour = I(data_var_ACP_var[,Vcol])), data = data_var_ACP_var, arrow = ggplot2::arrow(length = ggplot2::unit(0.25, "cm")))


    if(Var.label.repel==TRUE){


      PCA_var_graphic <- PCA_var_graphic+ggrepel::geom_text_repel(ggplot2::aes(x=data_var_ACP_var[,VD1]*(k+(1/10*k)), y=data_var_ACP_var[,VD2]*(k+(1/10*k)),
                                                             label=data_var_ACP_var[,Var.names_column], color=I(data_var_ACP_var[,Vcol.text])), size = kz, data=data_var_ACP_var)

    }else if(Var.label.repel==FALSE){


      PCA_var_graphic <- PCA_var_graphic+ggplot2::geom_text(ggplot2::aes(x=data_var_ACP_var[,VD1]*(k+(1/10*k)), y=data_var_ACP_var[,VD2]*(k+(1/10*k)),
                                                             label=data_var_ACP_var[,Var.names_column], color = I(data_var_ACP_var[,Vcol.text])), size = kz, data=data_var_ACP_var)

    }



  PCA_var_graphic <- PCA_var_graphic+ggplot2::ggtitle(var.title)+
    egg::theme_article()+
    ggplot2::theme(strip.background =ggplot2::element_rect(fill="white"))+
    ggplot2::theme(strip.text = ggplot2::element_text(colour = 'black'))+

    ggplot2::geom_segment(ggplot2::aes(x = -1*k, y = 0, xend = 1*k, yend = 0), color=col.circle.var.PCA, alpha=0.7)+
    ggplot2::geom_segment(ggplot2::aes(x = 0, y = -1*k, xend = 0, yend = 1*k), color=col.circle.var.PCA, alpha=0.7)+

    ggplot2::xlab(d1)+
    ggplot2::ylab(d2)

  PCA_var_graphic <- PCA_var_graphic+ggplot2::coord_fixed()

  PCA_var_graphic <<- PCA_var_graphic



  #Save the files

  if(Get.generated.data.frame==TRUE){

  Individuals_PCA_data <- data_ind_ACP
  Variables_PCA_data <- data_var_ACP_var
  Barycenter_PCA_data <- bie_bary
  Segments_PCA_data <- barycentre_ind

  Individuals_PCA_data <<- data_ind_ACP
  Variables_PCA_data <<- data_var_ACP_var
  Barycenter_PCA_data <<- bie_bary
  Segments_PCA_data <<- barycentre_ind

  }


  #Biplot.PCA
  if(missing(Biplot.PCA)){

    Biplot.PCA <- FALSE

  }

  if(Biplot.PCA==TRUE){


    #Adjust the PCA variable dimensions

    min.x <- min(data_ind_ACP[,Dima2])
    max.x <- max(data_ind_ACP[,Dima2])

    min.y <- min(data_ind_ACP[,Dimb2])
    max.y <- max(data_ind_ACP[,Dimb2])

    ray.x <- round((max.x-min.x)+(0.1*(max.x-min.x)),2)
    ray.y <- round((max.y-min.y)+(0.1*(max.y-min.y)),2)

    if(ray.x >= ray.y){

      k2 <- (ray.x/2)*k

    }else if(ray.y >= ray.x){

      k2 <- (ray.y/2)*k

    }

    if(Display.quanti.supp==TRUE & is.character(Var.quanti.supp)==TRUE){

      data_var_ACP_var$my.col <- col.arrow.var.PCA
      data_var_ACP_var.col2 <- which(data_var_ACP_var$Var.names %in% var.quanti.names.pca)
      data_var_ACP_var[data_var_ACP_var.col2,7] <- col.arrow.var.supp.PCA

      Vcol <- which(names(data_var_ACP_var) %in% "my.col")

      data_var_ACP_var$my.col.text <- col.text.var.PCA
      Vcol.text <- which(names(data_var_ACP_var) %in% "my.col.text")
      data_var_ACP_var[data_var_ACP_var.col2,8] <- col.arrow.var.supp.PCA

    }else if(Display.quanti.supp==FALSE | is.character(Var.quanti.supp)==FALSE){

      data_var_ACP_var$my.col <- col.arrow.var.PCA
      Vcol <- which(names(data_var_ACP_var) %in% "my.col")

      data_var_ACP_var$my.col.text <- col.text.var.PCA
      Vcol.text <- which(names(data_var_ACP_var) %in% "my.col.text")

    }

    Biplot_PCA <- ggplot2::ggplot()+
      ggforce::geom_circle(ggplot2::aes(x0 = 0, y0 = 0, r = 1*k2), color = col.circle.var.PCA, linetype = 2, alpha = 0.5)+
      ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = data_var_ACP_var[,VD1]*k2, yend = data_var_ACP_var[,VD2]*k2,
                                         colour = I(data_var_ACP_var[,Vcol])), data = data_var_ACP_var, arrow = ggplot2::arrow(length = ggplot2::unit(0.25, "cm")))


    if(Var.label.repel==TRUE){


      Biplot_PCA <- Biplot_PCA+ggrepel::geom_text_repel(ggplot2::aes(x=data_var_ACP_var[,VD1]*(k2+(1/10*k2)), y=data_var_ACP_var[,VD2]*(k2+(1/10*k2)),
                                                                               label=data_var_ACP_var[,Var.names_column], color=I(data_var_ACP_var[,Vcol.text])), size = kz, data=data_var_ACP_var)

    }else if(Var.label.repel==FALSE){


      Biplot_PCA <- Biplot_PCA+ggplot2::geom_text(ggplot2::aes(x=data_var_ACP_var[,VD1]*(k2+(1/10*k2)), y=data_var_ACP_var[,VD2]*(k2+(1/10*k2)),
                                                                         label=data_var_ACP_var[,Var.names_column], color = I(data_var_ACP_var[,Vcol.text])), size = kz, data=data_var_ACP_var)

    }



    Biplot_PCA <- Biplot_PCA+ggplot2::ggtitle(var.title)+
      egg::theme_article()+
      ggplot2::theme(strip.background =ggplot2::element_rect(fill="white"))+
      ggplot2::theme(strip.text = ggplot2::element_text(colour = 'black'))+

      ggplot2::geom_segment(ggplot2::aes(x = -1*k2, y = 0, xend = 1*k2, yend = 0), color=col.circle.var.PCA, alpha=0.7)+
      ggplot2::geom_segment(ggplot2::aes(x = 0, y = -1*k2, xend = 0, yend = 1*k2), color=col.circle.var.PCA, alpha=0.7)+

      ggplot2::xlab(d1)+
      ggplot2::ylab(d2)+
      ggplot2::geom_segment(ggplot2::aes(x = barycentre_ind[,Dima3], y = barycentre_ind[,Dimb3], xend = barycentre_ind[,Dima4], yend = barycentre_ind[,Dimb4]), linetype=2,linewidth=SLS, data = barycentre_ind, color = SLC)

    if(Ellipse.IC.95==FALSE){

      Biplot_PCA <- Biplot_PCA + ggforce::geom_ellipse(ggplot2::aes(x0 = bie[,Dima4], y0 = bie[,Dimb4],
                                                                              a = (bie[,facteur_IC95.1_column]), b = (bie[,facteur_IC95.2_column]),
                                                                              angle = 0, color=I(bie[,Ecol_column])),fill=NA, alpha=0.1, data=bie)

    }else if(Ellipse.IC.95==TRUE){


      if(factor.col.border.ellipse!=FALSE){


        Biplot_PCA <- Biplot_PCA + ggforce::geom_ellipse(ggplot2::aes(x0 = bie[,Dima4], y0 = bie[,Dimb4],
                                                                                a = (bie[,facteur_IC95.1_column]), b = (bie[,facteur_IC95.2_column]),
                                                                                angle = 0,fill=bie[,Efill_column], color=bie[,CBE_column]), alpha=0.1, data=bie, size=0.1, linetype=ELT)


      }else if(factor.col.border.ellipse==FALSE){

        Biplot_PCA <- Biplot_PCA + ggforce::geom_ellipse(ggplot2::aes(x0 = bie[,Dima4], y0 = bie[,Dimb4],
                                                                                a = (bie[,facteur_IC95.1_column]), b = (bie[,facteur_IC95.2_column]),
                                                                                angle = 0,fill=bie[,Efill_column]), color="azure4", alpha=0.1, data=bie, size=0.1, linetype=ELT)

      }
    }

    if(factor.colors==FALSE){
      data_ind_ACP[,FC] <- "black"
    }


    Biplot_PCA <- Biplot_PCA + ggplot2::geom_point(ggplot2::aes(x=data_ind_ACP[,Dima2], y=data_ind_ACP[,Dimb2], fill=data_ind_ACP[,FC], shape=data_ind_ACP[,FS], size=data_ind_ACP[,IPZ_column]), data=data_ind_ACP, alpha=1)+
      ggplot2::geom_point(ggplot2::aes(x=data_ind_ACP[,Dima2], y=data_ind_ACP[,Dimb2], fill=data_ind_ACP[,FC], shape=data_ind_ACP[,FS], size=data_ind_ACP[,IPZ_column]), data=data_ind_ACP, fill=NA, colour="black")+
      ggplot2::geom_point(ggplot2::aes(x=barycentre[,Dima5], y=barycentre[,Dimb5], fill=barycentre[,BCZfill_column], size=barycentre[,BCZ_column]), colour="black", shape=23, data = barycentre,show.legend = F)+
      ggplot2::scale_shape_manual(values = vector_shapes)


    if(factor.sizes==FALSE){

      Biplot_PCA <- Biplot_PCA+ggplot2::labs(fill=factor.colors.legend, shape=factor.shapes.legend)+
        ggplot2::guides(fill=ggplot2::guide_legend(override.aes = list(shape=21)))+
        ggplot2::scale_size_identity()+ggplot2::guides(size = "none")

    }else if(factor.sizes!=F){

      Biplot_PCA <- Biplot_PCA+ggplot2::labs(fill=factor.colors.legend, shape=factor.shapes.legend, size=factor.sizes.legend)+
        ggplot2::guides(fill=ggplot2::guide_legend(override.aes = list(shape=21)))+
        ggplot2::scale_size_identity()

    }else{NULL}

    if(factor.colors==FALSE){

      Biplot_PCA <- Biplot_PCA+ggplot2::guides(fill = "none")

    }else if(factor.colors!=FALSE){

      Biplot_PCA <- Biplot_PCA

    }else{NULL}


    if(factor.shapes==FALSE){

      Biplot_PCA <- Biplot_PCA+ggplot2::guides(shape = "none")

    }else if(factor.shapes!=FALSE){

      Biplot_PCA <- Biplot_PCA

    }else{NULL}

    Biplot_PCA <- Biplot_PCA+ ggplot2::ggtitle("PCA : biplot of individual and variable projections")+
      ggplot2::xlab(da)+
      ggplot2::ylab(db)+

      egg::theme_article()+
      ggplot2::theme(strip.background =ggplot2::element_rect(fill="white"))+
      ggplot2::theme(strip.text = ggplot2::element_text(colour = 'black'))

    if(is.character(color.palette)==TRUE){

      Biplot_PCA <- Biplot_PCA+ggplot2::scale_fill_manual(values=color.palette)

    }

    Biplot_PCA <- Biplot_PCA+ggplot2::coord_fixed()
    Biplot_PCA <<- Biplot_PCA


  }



  if(Biplot.PCA==FALSE){

     if(Heat.map.graph==FALSE & RDA.table.graph==FALSE){

      Separated_PCA_ind_var <- ggpubr::ggarrange(PCA_ind_graphic,PCA_var_graphic, nrow=1, ncol=2, widths = c(WidthIG,WidthVG))
      Separated_PCA_ind_var <<- Separated_PCA_ind_var
      Separated_PCA_ind_var

    }else if(Heat.map.graph==TRUE & RDA.table.graph==FALSE){

      if(Type.heat.map.graph=="square"){

        Separated_PCA_ind_var_HM <- ggpubr::ggarrange(PCA_ind_graphic,PCA_var_graphic,Pheatmap_var_graph, nrow=2, ncol=2, widths = c(WidthIG,WidthVG,WidthHM))
        Separated_PCA_ind_var_HM <<- Separated_PCA_ind_var_HM
        Separated_PCA_ind_var_HM

      }else if (Type.heat.map.graph=="circle"){

        Separated_PCA_ind_var_HM <- ggpubr::ggarrange(PCA_ind_graphic,PCA_var_graphic,Circle_heatmap_var_graph, nrow=2, ncol=2, widths = c(WidthIG,WidthVG,WidthHM))
        Separated_PCA_ind_var_HM <<- Separated_PCA_ind_var_HM
        Separated_PCA_ind_var_HM

      }

    }else if(Heat.map.graph==FALSE & RDA.table.graph==TRUE){

      Separated_PCA_ind_var <- ggpubr::ggarrange(PCA_ind_graphic,PCA_var_graphic, nrow=1, ncol=2, widths = c(WidthIG,WidthVG))
      Separated_PCA_ind_var_RDAtable <- ggpubr::ggarrange(Separated_PCA_ind_var, tab, nrow=2, ncol=1, heights = c(1,HeightRDA))
      Separated_PCA_ind_var_RDAtable <<- Separated_PCA_ind_var_RDAtable
      Separated_PCA_ind_var_RDAtable

    }else if(Heat.map.graph==TRUE & RDA.table.graph==TRUE){

      if(Type.heat.map.graph=="square"){

        Separated_PCA_ind_var_HM_RDAtable <- ggpubr::ggarrange(PCA_ind_graphic,PCA_var_graphic,Pheatmap_var_graph,tab, nrow=2, ncol=2,
                                                               widths = c(WidthIG,WidthVG,WidthHM), heights = c(1,1,1, HeightRDA))
        Separated_PCA_ind_var_HM_RDAtable <<- Separated_PCA_ind_var_HM_RDAtable
        Separated_PCA_ind_var_HM_RDAtable

      }else if (Type.heat.map.graph=="circle"){

        Separated_PCA_ind_var_HM_RDAtable <- ggpubr::ggarrange(PCA_ind_graphic,PCA_var_graphic,Circle_heatmap_var_graph,tab, nrow=2, ncol=2,
                                                               widths = c(WidthIG,WidthVG,WidthHM), heights = c(1,1,1, HeightRDA))
        Separated_PCA_ind_var_HM_RDAtable <<- Separated_PCA_ind_var_HM_RDAtable
        Separated_PCA_ind_var_HM_RDAtable

      }

    }else{NULL}

  }else if(Biplot.PCA==TRUE){

    if(Heat.map.graph==FALSE & RDA.table.graph==FALSE){

      Biplot_PCA

    }else if(Heat.map.graph==TRUE & RDA.table.graph==FALSE){

      if(Type.heat.map.graph=="square"){

        Biplot_PCA_HM <- ggpubr::ggarrange(Biplot_PCA,Pheatmap_var_graph, nrow=1, ncol=2, widths = c(1,WidthHM))
        Biplot_PCA_HM <<- Biplot_PCA_HM
        Biplot_PCA_HM

      }else if (Type.heat.map.graph=="circle"){

        Biplot_PCA_HM <- ggpubr::ggarrange(Biplot_PCA,Circle_heatmap_var_graph, nrow=1, ncol=2, widths = c(1,WidthHM))
        Biplot_PCA_HM <<- Biplot_PCA_HM
        Biplot_PCA_HM

      }

    }else if(Heat.map.graph==FALSE & RDA.table.graph==TRUE){

      Biplot_PCA_RDAtable <- ggpubr::ggarrange(Biplot_PCA, tab, nrow=1, ncol=2, heights = c(1,HeightRDA))
      Biplot_PCA_RDAtable <<- Biplot_PCA_RDAtable
      Biplot_PCA_RDAtable

    }else if(Heat.map.graph==TRUE & RDA.table.graph==TRUE){

      if(Type.heat.map.graph=="square"){


        Biplot_PCA_HM_RDAtable <- ggpubr::ggarrange(Pheatmap_var_graph,tab, nrow=1, ncol=2,
                                                    widths = c(WidthHM,1), heights = c(1, HeightRDA))

        Biplot_PCA_HM_RDAtable <- ggpubr::ggarrange(Biplot_PCA,Biplot_PCA_HM_RDAtable, nrow=2, ncol=1)
        Biplot_PCA_HM_RDAtable <<- Biplot_PCA_HM_RDAtable
        Biplot_PCA_HM_RDAtable

      }else if (Type.heat.map.graph=="circle"){

        Biplot_PCA_HM_RDAtable <- ggpubr::ggarrange(Circle_heatmap_var_graph,tab, nrow=1, ncol=2,
                                                    widths = c(WidthHM,1), heights = c(1, HeightRDA))

        Biplot_PCA_HM_RDAtable <- ggpubr::ggarrange(Biplot_PCA,Biplot_PCA_HM_RDAtable, nrow=2, ncol=1)
        Biplot_PCA_HM_RDAtable <<- Biplot_PCA_HM_RDAtable
        Biplot_PCA_HM_RDAtable

      }

    }else{NULL}

    }

}
