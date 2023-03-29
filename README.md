
# GABB

<!-- badges: start -->
<!-- badges: end -->

The goal of GABB package is to help the casual R users to perform and synthesize RDA and PCA analyses, from data set preparation to graphic visualization.
Ppackage GABB do not "re invent the wheel". Main inputs must be RDA and PCA objetcs created with vegan and FactoMiner respective packages.
For RDA : the package GABB goal is to check of data conformity for RDA and the facilitate the displaying/saving of RDA outputs.
For PCA : the package GABB goal is to facilitate and enhance the PCA graphic construction and display of individual/variable projections.
Graphics relies on ggplot2, ggplotify, ggforce and ggpubr packages. 
Created GABB plots are saved as grob/ggplot objects, so, users can modify them with classic ggplot2 options.

## Installation

You can install the development version of GABB like so:

``` r
# install.package("GABB")
# library(GABB)

```

## Example

Example of GABB functions on R based mtcars dataset:

``` r
library(GABB)
library(vegan)
library(FactoMiner)


## Example of GABB package pipeline with the base data.set "mtcars" 
my.data <- mtcars


## Data preparation for RDA and PCA : tranformation and scaling of numeric/quantitative variables

prep_data(data = my.data, quantitative_columns = c(1:7), transform_data_method = "log", scale_data = T)


## Check that factor identified meet RDA requirements
check_data_for_RDA(data_quant = data_quant,initial_data = my.data,factor_names = c("vs", "am", "gear", "carb"))
  # => factor "carb" doesn't meet RDA requirements (pval < 0.05)
  # simple solution : not considering it

check_data_for_RDA(data_quant = data_quant,initial_data = my.data,factor_names = c("vs", "am", "gear"))
  # all considered factors meet RDA requirements


## Performing RDA
library(vegan)
my.rda <- vegan::rda(data_quant~vs+am+gear, data=my.data)


## Display and save RDA output synthesis
RDA_outputs_synthesis(RDA = my.RDA, MVAsynth = T, MVAanova = F, RDATable = T)
  # => factor "gear" and factor interactions are not significant for the variation of model residual variance


## Performing PCA
library(FactoMineR)
my.pca <- FactoMineR::PCA(X = data_quant) 


## Create, display and save graphic output of individual and variable PCA

#Basic output with minimum required parameters
PCA_RDA_graphics(complete.data.set = initial_data_with_quant_transformed, PCA.object = my.pca, factor.names = c("vs", "am", "gear", "carb"))

#Advanced outputs (image below)
PCA_RDA_graphics(complete.data.set = initial_data_with_quant_transformed, PCA.object = my.pca, factor.names = c("vs", "am", "gear", "carb"),
                 Barycenter = TRUE, Segments = TRUE, Ellipse.IC.95 = TRUE,
                 Barycenter.Ellipse.Fac1 = "vs", Barycenter.Ellipse.Fac2 = "am",
                 factor.colors = "vs", factor.shapes = "am",
                 Barycenter.factor.col = "vs", Barycenter.factor.shape = "am",
                 Heat.map.graph = TRUE, Dims.heat.map = c(1:5), Cluster.row.heat.map = TRUE,
                 RDA.object = my.rda, RDA.table.graph = TRUE, RDA.table.graph.height = 10)
```
![Rplot01](https://user-images.githubusercontent.com/46051356/228469369-3cb8e6eb-d96e-4c3d-b1da-6f0da619b9d2.png)
```






