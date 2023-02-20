# GABB
R package for RDA and PCA analyses help, from data preparation to nice graphics

The goal of GABB package is to help the casual R users to perform and synthesize RDA and PCA analyses, from data set preparation to graphic visualization.

Package GABB do not "re invent the wheel". Main inputs must be RDA and PCA objetcs created with vegan and FactoMiner respective packages.

For RDA : the package GABB goal is to check of data conformity for RDA and the facilitate the displaying/saving of RDA outputs.
For PCA : the package GABB goal is to facilitate and enhance the PCA graphic construction and display of individual/variable projections.

Graphics relies on ggplot2, ggplotify, ggforce and ggpubr packages. 

Created GABB plots for RDA and PCA with multiple options : 
- Displaying the PCA individual with barycenters and confidence intervall ellipses calculated on up to 3 factors
- Displaying of the heatmap of PCA variable desired parameters (correlation, contribution, cos2, coordinates) correlation with dimensions 
- Displaying of the RDA table analysis
- ...

Created graphics are saved as grob/ggplot objects, so, users can modify them with classic ggplot2 options.

## Installation

You can install the development version of GABB like so:
Here from Github or:

``` r
# install.package("GABB")
# library(GABB)

```

## Example

This is a basic example which shows you how use GABB with the mtcars data.set:

``` r
library(GABB)
library(vegan)
library(FactoMiner)


#Example of GABB package pipeline with the base data.set "mtcars" 
my.data <- mtcars

#Data preparation for RDA and PCA : tranformation and scaling of numeric/quantitative variables

prep_data(data = my.data, quantitative_columns = c(1:7), transform_data_method = "log", scale_data = T)

check_data_for_RDA(data_quant = data_quant,initial_data = my.data,factor_names = c("vs", "am", "gear", "carb"))
# => the data set factor carb not suited for RDA analysis
# Rough simple solution : not considering it

check_data_for_RDA(data_quant = data_quant,initial_data = my.data,factor_names = c("vs", "am", "gear"))
#Check passed. Go for RDA.

#Performing simple RDA analysis
library(vegan)
my.RDA <- vegan::rda(data_quant~vs+am+gear, data=my.data)

#Display and save RDA outputs
RDA_outputs_synthesis(RDA = my.RDA, MVAsynth = T, MVAanova = F, RDATable = T)
# => the factor gear is not significant for the model residual variance modulations.
# For the following graphic displays, it will be considered as low importance factor.

#Performing simple PCA analysis
library(FactoMineR)
my.PCA <- FactoMineR::PCA(X = data_quant, scale.unit = F, ncp = 5, graph = F) 

#Create, display and save graphics of PCA individual and variable projections.
PCA_RDA_graphics(data = my.data, factor.names = c("vs", "am", "gear"), 
                 PCA.object = my.PCA, Dim.a = 1, Dim.b = 2,
                 Barycenter = T, Segments = T, Barycenter.min.size = 2, Ind.min.size = 1,
                 Segment.line.type = 2,Segment.line.size = 0.1,
                 Ellipse.IC.95 = T, Ellipse.Fac.1 = "vs", Ellipse.Fac.2 = "am",
                 factor.colors = "vs", factor.shapes = "am",
                 Var.circle = T, Var.circle.size = 2, Var.label.size = 5,
                 Overlaying.graphs = F, width.PCA.ind.graph = 0.6, width.PCA.var.graph = 0.4, 
                 Heat.map.graph = T, width.heat.map.graph = 0.3,var.parameter.heat.map = "cor",
                 Dims.heat.map = c(1,2),Display.cell.values.heat.map = T,Cluster.col.heat.map = T,Cluster.row.heat.map = T,
                 RDA.table.graph = T,RDA.table.graph.height = 0.3 )
  
``` 



![mtcars GABB ex](https://user-images.githubusercontent.com/46051356/220125152-5216451d-01c0-43ca-a8b7-b2c15f735407.png)
