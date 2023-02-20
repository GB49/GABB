#' PCA_RDA_graphics function
#' This function facilitate the user creation of PCA (! from FactoMineR package !) enhanced graphics with multiple options.
#' Individuals and variables graphics are possible. With or without variable cor, cos, contrib.. correlation to dimensions.
#' RDA outputs are displayable as table under graphics.
#'
#'
#' @param data Original data frame used for the PCA
#' @param factor.names Character vector of considered factor variables of PCA
#' @param sample.column Numeric or name (character) of the individual sample column. Default is data frame row names.
#' @param PCA.object The PCA object, built from package FactoMineR. PCA.object <- FactoMineR::PCA(mtcars_quant, scale.unit = T, ncp = 5, graph = F)
#' @param Dim.a Numeric value (1 ; 2 ...) of the first PCA dimension selected for graphic outputs.
#' @param Dim.b Numeric value (1 ; 2 ...) of the first PCA dimension selected for graphic outputs.
#' @param Barycenter TRUE or FALSE. TRUE : Calculate and Display the barycenter of individuals for Ellipse.Fac.1 and/or .2 and/or .3.
#' @param Segments TRUE or FALSE. TRUE : Display the linking segments between individuals and barycenters.
#' @param Barycenter.min.size Numeric. Minimum size of barycenter point projections. Ignore if Barycenter = FALSE
#' @param Ind.min.size Numeric. Minimum size of individuals point projections.
#' @param Segment.line.type Numeric. Type of segment lines (see ggplot2 line type). Ignore if Segments = FALSE
#' @param Segment.line.size Numeric. Minimum size of segment lines. Ignore if Segments = FALSE
#' @param Ellipse.IC.95 TRUE or FALSE. TRUE : Calculate and Display the Ellipse (95% mean confidence intervals) of individuals for Ellipse.Fac.1 and/or .2 and/or .3.
#' @param Ellipse.Fac.1 Character. Name of 1st factor/data frame column for Barycenter / Ellipses calculation.
#' @param Ellipse.Fac.2 Character. Name of 2nd factor/data frame column for Barycenter / Ellipses calculation.
#' @param Ellipse.Fac.3 Character. Name of 3rd factor/data frame column for Barycenter / Ellipses calculation.
#' @param factor.colors Character. Name of the factor/column considered for individuals colors.
#' @param factor.shapes Character. Name of the factor/column considered for individuals shapes.
#' @param factor.sizes Character. Name of the factor/column considered for individuals colors.
#' @param Var.circle TRUE or FALSE. TRUE = Display the PCA variable circle projection.
#' @param Var.circle.size Numeric. Value for increasing the size of Var.circle graphic.
#' @param Var.label.size Numeric. Value for increasing the size of Var.circle graphic labels.
#' @param Overlaying.graphs TRUE or FALSE. TRUE = Overlaying of PCA individuals and variables graphics. Default is set to FALSE.
#' @param width.PCA.ind.graph Numeric. Width ratio for PCA individuals graphic.
#' @param width.PCA.var.graph Numeric. Width ratio for PCA variables graphic.
#' @param width.heat.map.graph Numeric. Width ratio for Heat map variables graphic.
#' @param Heat.map.graph TRUE or FALSE. TRUE = Display the heat map of variable X parameter correlation to dimension.
#' @param var.parameter.heat.map Character. Parameter selected for the heat map correlation of Variable parameter to dimensions. values : "cor", "cos2", "coor","contrib".
#' @param Dims.heat.map Numeric. Numeric vector c(1,2) of dimensions considered for the variable parameter correlation.
#' @param Display.cell.values.heat.map TRUE or FALSE. TRUE = Display the rounded value of correlations within heat map cells.
#' @param Cluster.col.heat.map TRUE or FALSE. TRUE = cluster heatmap columns / dimensions.
#' @param Cluster.row.heat.map TRUE or FALSE. TRUE = cluster heatmap rows / quantitative variables.
#' @param RDA.table.graph TRUE or FALSE. TRUE = Display the RDA outputs table under PCA graphics.
#' @param RDA.table.graph.height Numeric. Set the ratio of RDA table graphic height. Defaut is set to 1.
#'
#' @return Several graphics
#' @export
#'
#' @examples
#'
#' \dontrun{
#' PCA_RDA_graphics(data=mtcars, factor.names=c("gears","carb"),PCA.object=respca, Dim.a=1, Dim.b=2,
#' Barycenter=TRUE, Segments=TRUE,Barycenter.min.size=2)
#' }
#'
#'
#




PCA_RDA_graphics <- function(data, factor.names, sample.column,
                             PCA.object, Dim.a, Dim.b, Barycenter, Segments,
                             Barycenter.min.size, Ind.min.size, Segment.line.type, Segment.line.size,
                             Ellipse.IC.95, Ellipse.Fac.1, Ellipse.Fac.2, Ellipse.Fac.3,
                             factor.colors, factor.shapes, factor.sizes,
                             Var.circle, Var.circle.size, Var.label.size, Overlaying.graphs,
                             width.PCA.ind.graph, width.PCA.var.graph, width.heat.map.graph,
                             Heat.map.graph, var.parameter.heat.map, Dims.heat.map, Display.cell.values.heat.map,
                             Cluster.col.heat.map, Cluster.row.heat.map, RDA.table.graph, RDA.table.graph.height){


  #Get columns number of factors
  colnumber <- which(names(data)%in%factor.names)

  #Extract coords for individuals and variables
  data_ind_ACP <- cbind(data.frame(cbind(PCA.object$ind$coord[,c(1:5)])), data[,c(colnumber)])
  data_var_ACP <- cbind(data.frame(cbind(PCA.object$var$coord[,c(1:5)])))

  #Define considered "factor columns" as factors

  colnumber2 <- which(names(data_ind_ACP)%in%factor.names)
  for (i in unique(colnumber2)){

    data_ind_ACP[,i] <- as.factor(data_ind_ACP[,i])

  }

  #Sample names : individuals

  if(missing(sample.column)==T){

    data_ind_ACP$Samples <- row.names(data)

  }else if(is.numeric(sample.column)==T){

    data_ind_ACP$Samples <- data[,sample.column]

  }else if(is.character(sample.column)==T){

    colnumbersample <- which(names(data)%in%sample.column)
    data_ind_ACP$Samples <- data[,colnumbersample]

  }else{NULL}

  #Variables names
  data_var_ACP$Var.names <- rownames(data_var_ACP)
  Var.names_column <- which(names(data_var_ACP)%in%"Var.names")


  #Barycenter calculation
  Dima <- paste("Dim.",Dim.a,sep="")
  Dimb <- paste("Dim.",Dim.b,sep="")

  Dima2 <- which(names(data_ind_ACP)%in%Dima)
  Dimb2 <- which(names(data_ind_ACP)%in%Dimb)

  if(missing(Ellipse.Fac.2)==TRUE & missing(Ellipse.Fac.3)==TRUE){

    F1 <- which(names(data_ind_ACP)%in%Ellipse.Fac.1)
    min_comb <- as.numeric(min(summary(data_ind_ACP[,F1])))
    max_comb <- as.numeric(max(summary(data_ind_ACP[,F1])))

    if(min_comb <= 1){

      cat("Warnings: WRONG factor choices used for ellipses representation (1 unique individual / factor combination)")

    }else if(min_comb < 3 & min_comb > 1){

      cat("Warnings: problematic factor choices used for ellipses representation (less than 3 unique individuals / factor combination)")

    }

    if(min_comb >= 2){

      barycentre1<-stats::aggregate(data_ind_ACP[,Dima2]~data_ind_ACP[,F1], data = data_ind_ACP, FUN = mean)
      names(barycentre1)[ncol(barycentre1)] <- Dima

      barycentre2<-stats::aggregate(data_ind_ACP[,Dimb2]~data_ind_ACP[,F1], data = data_ind_ACP, FUN = mean)
      names(barycentre2)[ncol(barycentre2)] <- Dimb

      barycentre<-dplyr::inner_join(barycentre1, barycentre2)
      names(barycentre)[1] <- Ellipse.Fac.1
      names(barycentre)[c((ncol(barycentre)-1):ncol(barycentre))]<-paste(names(barycentre)[c((ncol(barycentre)-1):ncol(barycentre))], "b", sep="_") #Numero des colonnes
      barycentre_ind<-dplyr::left_join(data_ind_ACP, barycentre)

      #Calcul de l'intervalle de confiance 95%
      IC95_sd <- stats::aggregate(data_ind_ACP[,Dima2]~data_ind_ACP[,F1], data = data_ind_ACP, FUN = stats::sd) ; names(IC95_sd)[2] <- "sd.1"
      names(IC95_sd)[1] <- Ellipse.Fac.1

      IC95_n <-stats::aggregate(data_ind_ACP[,Dima2]~data_ind_ACP[,F1], data = data_ind_ACP, FUN = length) ; names(IC95_n)[2] <- "nb.1"
      names(IC95_n)[1] <- Ellipse.Fac.1

      IC95_1 <- dplyr::inner_join(IC95_sd,IC95_n)

      IC95_sd <-stats::aggregate(data_ind_ACP[,Dimb2]~data_ind_ACP[,F1], data = data_ind_ACP, FUN = stats::sd) ; names(IC95_sd)[2] <- "sd.2"
      names(IC95_sd)[1] <- Ellipse.Fac.1

      IC95_n <-stats::aggregate(data_ind_ACP[,Dimb2]~data_ind_ACP[,F1], data = data_ind_ACP, FUN = length) ; names(IC95_n)[2] <- "nb.2"
      names(IC95_n)[1] <- Ellipse.Fac.1

      IC95_2 <- dplyr::inner_join(IC95_sd,IC95_n)

      barycentre_ind<-dplyr::inner_join(barycentre_ind, IC95_1)
      barycentre_ind<-dplyr::inner_join(barycentre_ind, IC95_2)

      barycentre_ind$facteur_IC95.1 <- ((1.96*barycentre_ind$sd.1)/sqrt(barycentre_ind$nb.1))
      barycentre_ind$facteur_IC95.2 <- ((1.96*barycentre_ind$sd.2)/sqrt(barycentre_ind$nb.2))

    }

  }else if(missing(Ellipse.Fac.2)==FALSE & missing(Ellipse.Fac.3)==TRUE){

    F1 <- which(names(data_ind_ACP)%in%Ellipse.Fac.1)
    F2 <- which(names(data_ind_ACP)%in%Ellipse.Fac.2)

    data_ind_ACP$nb_comb <- as.factor(paste(data_ind_ACP[,F1], data_ind_ACP[,F2]))

    min_comb <- as.numeric(min(summary(data_ind_ACP[,ncol(data_ind_ACP)])))
    max_comb <- as.numeric(max(summary(data_ind_ACP[,ncol(data_ind_ACP)])))

    if(min_comb <= 1){

      cat("Warnings: WRONG factor choices used for ellipses representation (1 unique individual / factor combination)")

    }else if(min_comb < 3 & min_comb > 1){

      cat("Warnings: problematic factor choices used for ellipses representation (less than 3 unique individuals / factor combination)")

    }else{NULL}

    if(min_comb >= 2){

      barycentre1<-stats::aggregate(data_ind_ACP[,Dima2]~data_ind_ACP[,F1]+data_ind_ACP[,F2], data = data_ind_ACP, FUN = mean)
      names(barycentre1)[ncol(barycentre1)] <- Dima

      barycentre2<-stats::aggregate(data_ind_ACP[,Dimb2]~data_ind_ACP[,F1]+data_ind_ACP[,F2], data = data_ind_ACP, FUN = mean)
      names(barycentre2)[ncol(barycentre2)] <- Dimb

      barycentre<-dplyr::inner_join(barycentre1, barycentre2)
      names(barycentre)[1] <- Ellipse.Fac.1
      names(barycentre)[2] <- Ellipse.Fac.2

      names(barycentre)[c((ncol(barycentre)-1):ncol(barycentre))]<-paste(names(barycentre)[c((ncol(barycentre)-1):ncol(barycentre))], "b", sep="_") #Numero des colonnes
      barycentre_ind<-dplyr::left_join(data_ind_ACP, barycentre)

      #Calcul de l'intervalle de confiance 95%
      IC95_sd <-stats::aggregate(data_ind_ACP[,Dima2]~data_ind_ACP[,F1]+data_ind_ACP[,F2], data = data_ind_ACP, FUN = stats::sd) ; names(IC95_sd)[3] <- "sd.1"
      names(IC95_sd)[1] <- Ellipse.Fac.1
      names(IC95_sd)[2] <- Ellipse.Fac.2

      IC95_n <-stats::aggregate(data_ind_ACP[,Dima2]~data_ind_ACP[,F1]+data_ind_ACP[,F2], data = data_ind_ACP, FUN = length) ; names(IC95_n)[3] <- "nb.1"
      names(IC95_n)[1] <- Ellipse.Fac.1
      names(IC95_n)[2] <- Ellipse.Fac.2

      IC95_1 <- dplyr::inner_join(IC95_sd,IC95_n)

      IC95_sd <-stats::aggregate(data_ind_ACP[,Dimb2]~data_ind_ACP[,F1]+data_ind_ACP[,F2], data = data_ind_ACP, FUN = stats::sd) ; names(IC95_sd)[3] <- "sd.2"
      names(IC95_sd)[1] <- Ellipse.Fac.1
      names(IC95_sd)[2] <- Ellipse.Fac.2

      IC95_n <-stats::aggregate(data_ind_ACP[,Dimb2]~data_ind_ACP[,F1]+data_ind_ACP[,F2], data = data_ind_ACP, FUN = length) ; names(IC95_n)[3] <- "nb.2"
      names(IC95_n)[1] <- Ellipse.Fac.1
      names(IC95_n)[2] <- Ellipse.Fac.2
      IC95_2 <- dplyr::inner_join(IC95_sd,IC95_n)

      barycentre_ind<-dplyr::inner_join(barycentre_ind, IC95_1)
      barycentre_ind<-dplyr::inner_join(barycentre_ind, IC95_2)

      barycentre_ind$facteur_IC95.1 <- ((1.96*barycentre_ind$sd.1)/sqrt(barycentre_ind$nb.1))
      barycentre_ind$facteur_IC95.2 <- ((1.96*barycentre_ind$sd.2)/sqrt(barycentre_ind$nb.2))

    }
  }else if(missing(Ellipse.Fac.2)==FALSE & missing(Ellipse.Fac.3)==FALSE){

    F1 <- which(names(data_ind_ACP)%in%Ellipse.Fac.1)
    F2 <- which(names(data_ind_ACP)%in%Ellipse.Fac.2)
    F3 <- which(names(data_ind_ACP)%in%Ellipse.Fac.3)

    data_ind_ACP$nb_comb <- as.factor(paste(data_ind_ACP[,F1], data_ind_ACP[,F2], data_ind_ACP[,F3]))

    min_comb <- as.numeric(min(summary(data_ind_ACP[,ncol(data_ind_ACP)])))
    max_comb <- as.numeric(max(summary(data_ind_ACP[,ncol(data_ind_ACP)])))

    if(min_comb <= 1){

      cat("Warnings: WRONG factor choices used for ellipses representation (1 unique individual / factor combination)")

    }else if(min_comb < 3 & min_comb > 1){

      cat("Warnings: problematic factor choices used for ellipses representation (less than 3 unique individuals / factor combination)")

    }else{NULL}

    if(min_comb >= 2){

      barycentre1<-stats::aggregate(data_ind_ACP[,Dima2]~data_ind_ACP[,F1]+data_ind_ACP[,F2]+data_ind_ACP[,F3], data = data_ind_ACP, FUN = mean)
      names(barycentre1)[ncol(barycentre1)] <- Dima

      barycentre2<-stats::aggregate(data_ind_ACP[,Dimb2]~data_ind_ACP[,F1]+data_ind_ACP[,F2]+data_ind_ACP[,F3], data = data_ind_ACP, FUN = mean)
      names(barycentre2)[ncol(barycentre2)] <- Dimb

      barycentre<-dplyr::inner_join(barycentre1, barycentre2)
      names(barycentre)[1] <- Ellipse.Fac.1
      names(barycentre)[2] <- Ellipse.Fac.2
      names(barycentre)[3] <- Ellipse.Fac.3

      names(barycentre)[c((ncol(barycentre)-1):ncol(barycentre))]<-paste(names(barycentre)[c((ncol(barycentre)-1):ncol(barycentre))], "b", sep="_") #Numero des colonnes
      barycentre_ind<-dplyr::left_join(data_ind_ACP, barycentre)

      #Calcul de l'intervalle de confiance 95%
      IC95_sd <-stats::aggregate(data_ind_ACP[,Dima2]~data_ind_ACP[,F1]+data_ind_ACP[,F2]+data_ind_ACP[,F3], data = data_ind_ACP, FUN = stats::sd) ; names(IC95_sd)[4] <- "sd.1"
      names(IC95_sd)[1] <- Ellipse.Fac.1
      names(IC95_sd)[2] <- Ellipse.Fac.2
      names(IC95_sd)[3] <- Ellipse.Fac.3

      IC95_n <-stats::aggregate(data_ind_ACP[,Dima2]~data_ind_ACP[,F1]+data_ind_ACP[,F2]+data_ind_ACP[,F3], data = data_ind_ACP, FUN = length) ; names(IC95_n)[4] <- "nb.1"
      names(IC95_n)[1] <- Ellipse.Fac.1
      names(IC95_n)[2] <- Ellipse.Fac.2
      names(IC95_n)[3] <- Ellipse.Fac.3

      IC95_1 <- dplyr::inner_join(IC95_sd,IC95_n)

      IC95_sd <-stats::aggregate(data_ind_ACP[,Dimb2]~data_ind_ACP[,F1]+data_ind_ACP[,F2]+data_ind_ACP[,F3], data = data_ind_ACP, FUN = stats::sd) ; names(IC95_sd)[4] <- "sd.2"
      names(IC95_sd)[1] <- Ellipse.Fac.1
      names(IC95_sd)[2] <- Ellipse.Fac.2
      names(IC95_sd)[3] <- Ellipse.Fac.3

      IC95_n <-stats::aggregate(data_ind_ACP[,Dimb2]~data_ind_ACP[,F1]+data_ind_ACP[,F2]+data_ind_ACP[,F3], data = data_ind_ACP, FUN = length) ; names(IC95_n)[4] <- "nb.2"
      names(IC95_n)[1] <- Ellipse.Fac.1
      names(IC95_n)[2] <- Ellipse.Fac.2
      names(IC95_n)[3] <- Ellipse.Fac.3

      IC95_2 <- dplyr::inner_join(IC95_sd,IC95_n)

      barycentre_ind<-dplyr::inner_join(barycentre_ind, IC95_1)
      barycentre_ind<-dplyr::inner_join(barycentre_ind, IC95_2)

      barycentre_ind$facteur_IC95.1 <- ((1.96*barycentre_ind$sd.1)/sqrt(barycentre_ind$nb.1))
      barycentre_ind$facteur_IC95.2 <- ((1.96*barycentre_ind$sd.2)/sqrt(barycentre_ind$nb.2))

    }
  }else{NULL}

  #Arguments for plot
  #Individuals
  data_ind_ACP$IPZ <- 2

  if (missing(factor.sizes)==TRUE & missing(Ind.min.size)==FALSE){

    data_ind_ACP$IPZ <- Ind.min.size
    factor.sizes.forced <- 1

  }else if (missing(factor.sizes)==FALSE & missing(Ind.min.size)==FALSE){

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

  if (Barycenter==FALSE | missing(Barycenter)==TRUE & missing(Barycenter.min.size)==FALSE | missing(Barycenter)==TRUE & missing(Barycenter.min.size)==TRUE){

    barycentre$BCZfill <- NA
    barycentre$BCZcol <- NA

    barycentre$BCZ <- 0


  }else if (missing(Barycenter)==FALSE & missing(Barycenter.min.size)==TRUE){

    colBar <- which(names(barycentre)%in%factor.colors)

    barycentre$BCZfill <- barycentre[,colBar]
    barycentre$BCZ <- 3

  }else if (missing(Barycenter)==FALSE & missing(Barycenter.min.size)==FALSE){

    colBar <- which(names(barycentre)%in%factor.colors)

    barycentre$BCZfill <- barycentre[,colBar]
    barycentre$BCZ <- Barycenter.min.size
  }

  #Ellipses
  barycentre_ind$EFill <- "white"
  barycentre_ind$Ecol <- "black"

  if (missing(Ellipse.IC.95)==TRUE | Ellipse.IC.95==FALSE){

    barycentre_ind$EFill <- NA
    barycentre_ind$Ecol <- NA

  }else if (missing(Ellipse.IC.95)==FALSE & Ellipse.IC.95==TRUE){

    colBar <- which(names(barycentre_ind)%in%factor.colors)

    barycentre_ind$EFill <- barycentre_ind[,colBar]
    barycentre_ind$Ecol <- "black"

  }

  #Segments
  SLT <- Segment.line.type
  SLS <- Segment.line.size
  SLC <- "black"

  if(Segments==FALSE | missing(Segments)==TRUE){

    SLC = NA

  }


  if(missing(Var.label.size)==TRUE){
    kz <- 2.5
  }else if (missing(Var.label.size)==FALSE){
    kz <- Var.label.size
  }

  if(missing(Var.circle.size)==TRUE){
    k <- 2.5
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

  FC <- which(names(data_ind_ACP)%in%factor.colors)
  FC2 <- which(names(barycentre_ind)%in%factor.colors)
  FC3 <- which(names(barycentre)%in%factor.colors)

  FS <- which(names(data_ind_ACP)%in%factor.shapes)

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





  WidthIG <- width.PCA.ind.graph
  WidthVG <- width.PCA.var.graph
  WidthHM <- width.heat.map.graph

  #legends

  factor.colors.legend <- NA
  factor.shapes.legend <- NA
  factor.sizes.legend <- NA



  if(missing(factor.colors)==F){
    factor.colors.legend <- factor.colors
  }
  if(missing(factor.shapes)==F){
    factor.shapes.legend <- factor.shapes
  }
  if(missing(factor.sizes)==F){
    factor.sizes.legend <- factor.sizes.forced
  }

  #RDA table output as graph

  Table_RDA$Factor <- rownames(Table_RDA)
  Table_RDA <- Table_RDA[,c(1:7)]

  tab <- ggpubr::ggtexttable(Table_RDA, rows = NULL,theme = ggpubr::ttheme("classic"))+
         ggplot2::labs(title="RDA outputs")

  tab <<- tab

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

      print("Please choose valid PCA variable parameter (coord, cor, cos2 or contrib")

    }else if (gng=="ok"){

      DHM <- paste("Dim.",Dims.heat.map,sep="")
      colnumber_pheatmap <- which(names(matrixHM)%in%DHM)

      text_pheatmap <- matrixHM[,colnumber_pheatmap]

      if(missing(Display.cell.values.heat.map)==T | Display.cell.values.heat.map==F){
        colheatmap <- NA
      }else if(missing(Display.cell.values.heat.map)==F & Display.cell.values.heat.map==T){
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


      Pheatmap_var_graph <- ggplotify::as.ggplot(Pheatmap_var_graph)

      Pheatmap_var_graph <- Pheatmap_var_graph + egg::theme_article()+
        ggplot2::labs(title=paste("Heat map representation of variable ", textvar, " to dim", sep=""))+
        ggplot2::xlab("Dim1")+
        ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                       axis.ticks.y = ggplot2::element_blank(),
                       axis.text.y = ggplot2::element_blank(),

                       axis.title.x = ggplot2::element_text(colour=NA),
                       axis.ticks.x = ggplot2::element_line(colour=NA),
                       axis.text.x = ggplot2::element_text(colour=NA))

      Pheatmap_var_graph <<- Pheatmap_var_graph

    }else{NULL}
  }


  #Dummy objects, to be overwrite

  #Individuals PCA Plot
  BCZ_column <- which(names(barycentre)%in%"BCZ")
  BCZcol_column <- which(names(barycentre)%in%"BCZcol")
  BCZfill_column <- which(names(barycentre)%in%"BCZfill")

  Efill_column <- which(names(barycentre_ind)%in%"EFill")
  Ecol_column <- which(names(barycentre_ind)%in%"Ecol")

  facteur_IC95.1_column <- which(names(barycentre_ind)%in%"facteur_IC95.1")
  facteur_IC95.2_column <- which(names(barycentre_ind)%in%"facteur_IC95.2")

  IPZ_column <- which(names(data_ind_ACP)%in%"IPZ")


  PCA_ind_graphic <- ggplot2::ggplot()+
    ggplot2::geom_segment(ggplot2::aes(x = barycentre_ind[,Dima3], y = barycentre_ind[,Dimb3], xend = barycentre_ind[,Dima4], yend = barycentre_ind[,Dimb4]), linetype=2,linewidth=SLS, data = barycentre_ind, color = SLC)

  if(missing(Ellipse.IC.95)==TRUE | Ellipse.IC.95==F){

    PCA_ind_graphic <- PCA_ind_graphic + ggforce::geom_ellipse(ggplot2::aes(x0 = barycentre_ind[,Dima4], y0 = barycentre_ind[,Dimb4],
                                                                   a = (barycentre_ind[,facteur_IC95.1_column]), b = (barycentre_ind[,facteur_IC95.2_column]),
                                                                   angle = 0, color=I(barycentre_ind[,Ecol_column])),fill=NA, alpha=0.1, data=barycentre_ind)

  }else if(missing(Ellipse.IC.95)==F & Ellipse.IC.95==T){

    PCA_ind_graphic <- PCA_ind_graphic + ggforce::geom_ellipse(ggplot2::aes(x0 = barycentre_ind[,Dima4], y0 = barycentre_ind[,Dimb4],
                                                                   a = (barycentre_ind[,facteur_IC95.1_column]), b = (barycentre_ind[,facteur_IC95.2_column]),
                                                                   angle = 0,fill=barycentre_ind[,Efill_column], color=I(barycentre_ind[,Ecol_column])), alpha=0.1, data=barycentre_ind)


  }

  PCA_ind_graphic <- PCA_ind_graphic + ggplot2::geom_point(ggplot2::aes(x=data_ind_ACP[,Dima2], y=data_ind_ACP[,Dimb2], fill=data_ind_ACP[,FC], shape=data_ind_ACP[,FS], size=data_ind_ACP[,IPZ_column]), data=data_ind_ACP, alpha=1)+
    ggplot2::geom_point(ggplot2::aes(x=data_ind_ACP[,Dima2], y=data_ind_ACP[,Dimb2], fill=data_ind_ACP[,FC], shape=data_ind_ACP[,FS], size=data_ind_ACP[,IPZ_column]), data=data_ind_ACP, fill=NA, colour="black")+
    ggplot2::geom_point(ggplot2::aes(x=barycentre[,Dima5], y=barycentre[,Dimb5], fill=barycentre[,BCZfill_column], size=barycentre[,BCZ_column], colour=barycentre[,BCZcol_column]), shape=23, data = barycentre,show.legend = F)+
    ggplot2::scale_shape_manual(values = vector_shapes)

    if(missing(factor.sizes)==T){

    PCA_ind_graphic <- PCA_ind_graphic+ggplot2::labs(fill=factor.colors.legend, shape=factor.shapes.legend)+
    ggplot2::guides(fill=ggplot2::guide_legend(override.aes = list(shape=21)))+
    ggplot2::scale_size_identity()+ggplot2::guides(size = "none")

    }else if(missing(factor.sizes)==F){

    PCA_ind_graphic <- PCA_ind_graphic+ggplot2::labs(fill=factor.colors.legend, shape=factor.shapes.legend, size=factor.sizes.legend)+
    ggplot2::guides(fill=ggplot2::guide_legend(override.aes = list(shape=21)))+
    ggplot2::scale_size_identity()

    }else{NULL}

    PCA_ind_graphic <- PCA_ind_graphic+ ggplot2::ggtitle("PCA : Sample projection with 95% CI")+
    ggplot2::xlab(da)+
    ggplot2::ylab(db)+

    egg::theme_article()+
    ggplot2::theme(strip.background =ggplot2::element_rect(fill="white"))+
    ggplot2::theme(strip.text = ggplot2::element_text(colour = 'black'))

  PCA_ind_graphic <<- PCA_ind_graphic
  PCA_ind_graphic

  PCA_var_graphic <- ggplot2::ggplot()+
    ggforce::geom_circle(ggplot2::aes(x0 = 0, y0 = 0, r = 1*k), color = "grey", linetype = 2, alpha = 0.5)+
    ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = data_var_ACP[,VD1]*k, yend = data_var_ACP[,VD2]*k),
                          colour = "black", data = data_var_ACP, arrow = ggplot2::arrow(length = ggplot2::unit(0.25, "cm")))+
    ggplot2::geom_text(ggplot2::aes(x=data_var_ACP[,VD1]*(k+(1/10*k)), y=data_var_ACP[,VD2]*(k+(1/10*k)), label=data_var_ACP[,Var.names_column]), color = "black", size = kz, data=data_var_ACP)+



    ggplot2::ggtitle("PCA : Variable projection")+
    egg::theme_article()+
    ggplot2::theme(strip.background =ggplot2::element_rect(fill="white"))+
    ggplot2::theme(strip.text = ggplot2::element_text(colour = 'black'))+

    ggplot2::geom_segment(ggplot2::aes(x = -4, y = 0, xend = 4, yend = 0), color="grey", alpha=0.7)+
    ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 4, yend = 0), color="grey", alpha=0.7)+

    ggplot2::xlab(d1)+
    ggplot2::ylab(d2)

  PCA_var_graphic <<- PCA_var_graphic

  if(Overlaying.graphs==TRUE){

    Overlaying_PCA_ind_var <- ggplot2::ggplot()+
      ggplot2::geom_segment(ggplot2::aes(x = barycentre_ind[,Dima3], y = barycentre_ind[,Dimb3], xend = barycentre_ind[,Dima4], yend = barycentre_ind[,Dimb4]), linetype=2,linewidth=SLS, data = barycentre_ind, color = SLC)

    if(missing(Ellipse.IC.95)==TRUE | Ellipse.IC.95==F){

      Overlaying_PCA_ind_var <- Overlaying_PCA_ind_var + ggforce::geom_ellipse(ggplot2::aes(x0 = barycentre_ind[,Dima4], y0 = barycentre_ind[,Dimb4],
                                                                                   a = (barycentre_ind[,facteur_IC95.1_column]), b = (barycentre_ind[,facteur_IC95.2_column]),
                                                                                   angle = 0, color=I(barycentre_ind[,Ecol_column])),fill=NA, alpha=0.1, data=barycentre_ind)

    }else if(missing(Ellipse.IC.95)==F & Ellipse.IC.95==T){

      Overlaying_PCA_ind_var <- Overlaying_PCA_ind_var + ggforce::geom_ellipse(ggplot2::aes(x0 = barycentre_ind[,Dima4], y0 = barycentre_ind[,Dimb4],
                                                                                   a = (barycentre_ind[,facteur_IC95.1_column]), b = (barycentre_ind[,facteur_IC95.2_column]),
                                                                                   angle = 0,fill=barycentre_ind[,Efill_column], color=I(barycentre_ind[,Ecol_column])), alpha=0.1, data=barycentre_ind)


    }

    Overlaying_PCA_ind_var <- Overlaying_PCA_ind_var +ggplot2::geom_point(ggplot2::aes(x=data_ind_ACP[,Dima2], y=data_ind_ACP[,Dimb2], fill=data_ind_ACP[,FC], shape=data_ind_ACP[,FS], size=data_ind_ACP[,IPZ_column]), data=data_ind_ACP, alpha=1)+
      ggplot2::geom_point(ggplot2::aes(x=data_ind_ACP[,Dima2], y=data_ind_ACP[,Dimb2], fill=data_ind_ACP[,FC], shape=data_ind_ACP[,FS], size=data_ind_ACP[,IPZ_column]), data=data_ind_ACP, fill=NA, colour="black")+
      ggplot2::geom_point(ggplot2::aes(x=barycentre[,Dima5], y=barycentre[,Dimb5], fill=barycentre[,BCZfill_column], size=barycentre[,BCZ_column], colour=barycentre[,BCZcol_column]), shape=23, data = barycentre)+
      ggplot2::scale_shape_manual(values = vector_shapes)

    if(missing(factor.sizes)==T){

      PCA_ind_graphic <- PCA_ind_graphic+ggplot2::labs(fill=factor.colors.legend, shape=factor.shapes.legend)+
        ggplot2::guides(fill=ggplot2::guide_legend(override.aes = list(shape=21)))+
        ggplot2::scale_size_identity()+ggplot2::guides(size = "none")

    }else if(missing(factor.sizes)==F){

      PCA_ind_graphic <- PCA_ind_graphic+ggplot2::labs(fill=factor.colors.legend, shape=factor.shapes.legend, size=factor.sizes.legend)+
        ggplot2::guides(fill=ggplot2::guide_legend(override.aes = list(shape=21)))+
        ggplot2::scale_size_identity(guide=factor.sizes)

    }else{NULL}

    PCA_ind_graphic <- PCA_ind_graphic+ ggplot2::ggtitle("PCA : Sample projection with 95% CI")+
      ggplot2::xlab(da)+
      ggplot2::ylab(db)+

      egg::theme_article()+
      ggplot2::theme(strip.background =ggplot2::element_rect(fill="white"))+
      ggplot2::theme(strip.text = ggplot2::element_text(colour = 'black'))+

      ggplot2::geom_segment(ggplot2::aes(x = -4, y = 0, xend = 4, yend = 0), color="grey", alpha=0.7)+
      ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 4, yend = 0), color="grey", alpha=0.7)+

      ggplot2::xlab(d1)+
      ggplot2::ylab(d2)

    Overlaying_PCA_ind_var <<- Overlaying_PCA_ind_var
    Overlaying_PCA_ind_var

  }else if(Overlaying.graphs==FALSE){

    if(missing(Heat.map.graph)==TRUE | Heat.map.graph==F & missing(RDA.table.graph)==T | RDA.table.graph==F){

      Separated_PCA_ind_var <- ggpubr::ggarrange(PCA_ind_graphic,PCA_var_graphic, nrow=1, ncol=2, widths = c(WidthIG,WidthVG))
      Separated_PCA_ind_var <<- Separated_PCA_ind_var
      Separated_PCA_ind_var

    }else if(missing(Heat.map.graph)==F & Heat.map.graph==T & missing(RDA.table.graph)==T | RDA.table.graph==F){

      Separated_PCA_ind_var_HM <- ggpubr::ggarrange(PCA_ind_graphic,PCA_var_graphic,Pheatmap_var_graph, nrow=1, ncol=3, widths = c(WidthIG,WidthVG,WidthHM))
      Separated_PCA_ind_var_HM <<- Separated_PCA_ind_var_HM
      Separated_PCA_ind_var_HM

    }else if(missing(Heat.map.graph)==TRUE | Heat.map.graph==F & missing(RDA.table.graph)==F & RDA.table.graph==T){

      Separated_PCA_ind_var <- ggpubr::ggarrange(PCA_ind_graphic,PCA_var_graphic, nrow=1, ncol=2, widths = c(WidthIG,WidthVG))
      Separated_PCA_ind_var_RDAtable <- ggpubr::ggarrange(Separated_PCA_ind_var, tab, nrow=2, ncol=1, heights = c(1,HeightRDA))
      Separated_PCA_ind_var_RDAtable <<- Separated_PCA_ind_var_RDAtable
      Separated_PCA_ind_var_RDAtable

    }else if(missing(Heat.map.graph)==F & Heat.map.graph==T & missing(RDA.table.graph)==F & RDA.table.graph==T){

      Separated_PCA_ind_var_HM <- ggpubr::ggarrange(PCA_ind_graphic,PCA_var_graphic,Pheatmap_var_graph, nrow=1, ncol=3, widths = c(WidthIG,WidthVG,WidthHM))
      Separated_PCA_ind_var_HM_RDAtable <- ggpubr::ggarrange(Separated_PCA_ind_var_HM, tab, nrow=2, ncol=1, heights = c(1,HeightRDA))
      Separated_PCA_ind_var_HM_RDAtable <<- Separated_PCA_ind_var_HM_RDAtable
      Separated_PCA_ind_var_HM_RDAtable

    }else{NULL}

  }else{NULL}


}
