---
title: "Lab 2: Vancouver Crime Point Pattern Analysis"
author: "Geog 418"
date: "22/01/2024"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

##R Markdown requires that you have a an application installed on you machine that can run Latex. If you do not already have Latex on your computer, you can install it direction from R with the following code:

tinytex::install_tinytex()


## For this lab you will need the following libraries: 
##, raster, spatstat, tmap, knitr


library("raster")
library("spatstat")
library("tmap")
library("knitr")
library("sf")
library("ggplot2")
library("spatstat")
library("sp")
library("st")
library("dplyr")
library("plyr")



dir <- "C:/Users/chrisbone/OneDrive - University of Victoria/Courses/Geog 418 Spatial Analysis/2024/Assignment 2"
setwd(dir)

```

## Introduction

Vancouver, British Columbia is seeing a concerning rise in crime in 2020 so far, with violent crimes up 5.6% and commercial break ins up 47.9%, according to the Vancouver Police Department [1]. This rise in crime comes amidst the COVID-19 pandemic, a global event that has forced the re-organization of the daily routines of the many people worldwide. In Vancouver, this re-organization has largely taken the form of people spending more time at home, and businesses reducing their hours or closing altogether. Such shifts in the routine activities of the general public is thought to have a measurable effect on the spatial and temporal distribution of crime, as described by routine activity theory [2]. According to routine activity theory, a crime requires the convergence of a “likely offender”, a “suitable target” and “the absence of capable guardians” in time and space [2]. Given this theory, the increase in crime rates, and the dramatic re-arrangement of routine activities in Vancouver, it is highly relevant that the spatial dynamics of crime be studied in Vancouver at this time.

While there have been several studies conducted on the spatial distribution of crimes in Vancouver in the past [3,4,5], there is a lack of temporally relevant crime studies in the literature. Aside from scientific literature, the other main resource for information about crime in Vancouver is the VPD, who provide regular updates comparing the number of crime incidents from year to year. While these reports are useful, they do not provide significant detail about the spatial aspect of crime data. In addition to the need for updated scientific analysis of crime in Vancouver, there is also a more tangible, practical need for information about the location of recent crimes so as to ensure the most effective distribution of police resources. Without current information about crime hotspots, police may be focusing their energy in the wrong areas, putting public safety at risk.

The objective of this analysis is to provide insight into the spatial distribution of bicycle thefts and mischief crimes in 2020 through several different point pattern analysis tests. Generally speaking, these statistical tests all aim to answer the the following questions:

1. Are bicycle thefts and mischief crimes in Vancouver in 2020 randomly located?
2. If the crimes being studied are not found to be randomly located, what spatial pattern do they have?
3. If the data are clustered, where do those clusters occur?


```{r Data Cleaning, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE}
#####
##Read in and clean data
VanCity <- readOGR(".", 'local-area-boundary', verbose = FALSE)
VanCrime <- read.csv("./crimedata_csv_all_years.csv")

#clean up the columns
VanCrime_Clean <- VanCrime[which(VanCrime$YEAR == 2014),]
# range(VanCrime_Clean$YEAR)
# range(VanCrime_Clean$X)

#omit values with NA
VanCrime_Clean <- na.omit(VanCrime_Clean)
# range(VanCrime_Clean$X)

VanCrime_Clean <- VanCrime_Clean[which(VanCrime_Clean$X > 0), ]
# range(VanCrime_Clean$X)
# range(VanCrime_Clean$Y)

Coords <- VanCrime_Clean[,c("X", "Y")]
crs <- CRS("+init=epsg:32610")

#create a file type called a SpatialPointsDataFrame
VanCrimePoints <- SpatialPointsDataFrame(coords = Coords, data = VanCrime_Clean, proj4string = crs)

#transform the projection of both datasets to ensure that they are the same
VanCrimePoints <- spTransform(VanCrimePoints, CRS("+init=epsg:3005"))
VanCity <- spTransform(VanCity, CRS("+init=epsg:3005"))

#intersect the two datasets
VanCrimePoints <- raster::intersect(VanCrimePoints, VanCity)

#convert the crimes data type to factor
VanCrimePoints@data$TYPE <- as.factor(VanCrimePoints@data$TYPE)
# levels(VanCrimePoints$TYPE)

kma_crime1 <- VanCrimePoints[which(VanCrimePoints$TYPE == "Mischief"),]
kma_crime1$x <- coordinates(kma_crime1)[,1]
kma_crime1$y <- coordinates(kma_crime1)[,2]

kma_crime2 <- VanCrimePoints[which(VanCrimePoints$TYPE == "Break and Enter Residential/Other"),]
kma_crime2$x <- coordinates(kma_crime2)[,1]
kma_crime2$y <- coordinates(kma_crime2)[,2]

#check for and remove duplicated points
#first, finds zero distance among points to see if there are any duplicates
zd <- zerodist(kma_crime1)
# zd

zd <- zerodist(kma_crime2)
# zd

#if there are duplicates, remove them
kma_crime1 <- remove.duplicates(kma_crime1)
kma_crime2 <- remove.duplicates(kma_crime2)

#create an "extent" object which can be used to create the observation window for spatstat
kma1.ext <- as.matrix(extent(kma_crime1)) 
kma2.ext <- as.matrix(extent(kma_crime2)) 

#observation window
window1 <- as.owin(list(xrange = kma1.ext[1,], yrange = kma1.ext[2,]))
window2 <- as.owin(list(xrange = kma2.ext[1,], yrange = kma2.ext[2,]))

#create ppp oject from spatstat
kma1.ppp <- ppp(x = kma_crime1$x, y = kma_crime1$y, window = window1)
kma2.ppp <- ppp(x = kma_crime2$x, y = kma_crime2$y, window = window2)
```

## Study Area
As mentioned above, this study focuses on the distribution of bicycle theft and mischief crimes in Vancouver, BC. A map of the study area and crime points are shown below for reference (Figure 1).

```{r Study Area Map, echo=FALSE, eval=TRUE, warning=FALSE, fig.cap="Selected vancouver crimes in 2020."}
map_Crime <- tm_shape(VanCity) + #make the main shape
  tm_polygons(col = "gray50", border.col = "black") +  #fill polygons
  tm_shape((kma_crime2)) +
  tm_symbols(size = 0.05, col = "blue", shape = 20, alpha = 0.25) +
  tm_shape((kma_crime1)) +
  tm_symbols(size = 0.05, col = "pink", shape = 20, alpha = 0.25) +
  tm_layout(title = "Vancouver Crimes 2020", 
            title.position = c("LEFT", "BOTTOM"),
            legend.position = c("LEFT", "TOP")) +
  tm_add_legend(type = "symbol", 
              labels = c("Mischief", "Theft of bicycle"), 
              col = c(adjustcolor( "blue", alpha.f = 0.5), 
                      adjustcolor( "pink", alpha.f = 0.5)), 
              shape = c(20,20))

map_Crime
```

## Data

Data for this study were collected from the Vancouver Police Department’s crime database, retrieved from https://geodash.vpd.ca/opendata/. This dataset has been updated weekly since 2003 and contains a variety of information about the crime such as the crime type, the GPS location at which it occurred, the name of the 100 block that it occurred on, and the time at which it occurred, measured to the minute [6].

Though this dataset provided by the VPD is an extremely useful resource, it does have several limitations. One of these limitations is that the dataset does not include all types of crime; most notably, it is lacking data on violent crimes in Vancouver. Given this, it should be noted that Figure 1 displays only the selected crime types [6].  Another limitation of the data used for this analysis is that the geographic locations of the crimes are offset from their true locations for privacy protection reasons [6].


## Analysis

**Are Theft of Bicycle and Mischief crimes randomly located in Vancouver in 2020?**

To answer this question, this study employed three different statistical tests that use different methodologies to determine if the data are showing random, dispersed or clustered spatial patterns. The theory behind these tests, and their results for each crime type will be outlined in the following section.

### Nearest Neighbour Analysis

Nearest neighbour is a point pattern analysis method based on measuring the distance between a given point and the point closest to it, or its “nearest neighbour”. This nearest neighbour measurement is performed for all points in the dataset, and an average nearest neighbour distance is calculated. In order to determine the meaning of this average nearest neighbour value, it must be compared to the average nearest neighbour values that would be expected if the data in question were randomly distributed, clustered or dispersed.

The average nearest neighbour value for a spatially random distribution is calculated using the following equation:

$$
\bar{NND_R} = \frac{1}
$$

The density of points within the study area is placed in the denominator of nearest neighbour analyses for the purpose of standardization. For our analysis the area of the study area was determined using a boundary shapefile downloaed from the City of Vancouver open data portal. Next, the average nearest neighbour value for a perfectly dispersed pattern is given by:

$$
\bar{NND_D} = \frac{1.07453}{\sqrt{Density}}
$$

A formula is not needed to determine the value for a perfectly clustered distribution because this would result in an average nearest neighbour distance of 0.

The variables outlined above give “benchmarks” against which one can compare the average nearest neighbour value for the dataset in question, but in order to determine if the result of this statistical test is significantly different than random, we must perform a z test.  In the case of nearest neighbour analysis, a z-score is calculated using the formula:

$$
Z_n = \frac{\bar{NND} - \bar{NND_R}}{\sigma\bar{NND}}
$$

The resultant z-score from this formula can then be compared with the z-value for the significance level being used to determine if the results are statistically significant. For the current analysis, a significance level of 95% was used, which corresponds to a z-value of 1.96. For this analysis, all nearest neighbour distance values are expressed in meters.


### Quadrat Analysis

The next statistical test used in this study is quadrat analysis. Quadrat analysis is a test that allows one to divide the study area into equal sized cells (quadrats) and count the number of points (crimes, in this case) in each quadrat. For the current analysis, the study area (totalling ~118 square kilometres) was divided into 144 quadrats.  This number of quadrats was chosen due to the fact that it resulted in each quadrat having an area of approximately 1 square kilometre, which was thought to be the most effective scale at which to study the spatial distribution of bicyle theft and mischief crimes in Vancouver. After the number of points in each quadrat are calculated, a frequency table can be created using variables x, which is the number of points in the quadrat, and f, number of quadrats containing x number of points. These two variables are then used in the following equation:

$$
VAR = \frac{\Sigma f_ix_i^2 - [\frac{(\Sigma f_ix_i)^2}{m}]}{m-1}
$$

VAR, the variable being calculated in the above equation, represents variance, which is a measure that describes how much the number of points in each quadrat varies across the study area. For example, a perfectly dispersed set of points, the variance would be zero, as all quadrats would contain the same number of points.

Because the variance is highly influenced by the density of points collected within the study area, a way to standardize this value is needed if it is to be a meaningful statistic. In quadrat analysis, this is done through the calculation of a variance-to-mean ratio, or VMR. VMR is exactly what it sounds like: the variance divided by the mean number of points per quadrat. In quadrat analysis, the mean essentially acts as a proxy for a random distribution of points, meaning that the VMR value gives the analyst the ability to compare the variance seen in the study area with what would be expected for a random distribution for the same data given the number of points and quadrats involved. When the VMR equals 1, it means that the data are randomly distributed, while relatively low or high VMR values indicate dispersed and clustered distributions, respectively.

To test if the results of a quadrat analysis are significant, a chi-square test is performed, and the resultant p-value is compared with the p-value for the desired level of significance, in this case p = 0.05 (95% confidence).

### K-function

The k-function statistic is another way of measuring the spatial distribution of points within a given study area. This analysis technique is particularly useful for addressing the issue of scale in point pattern analyses, as it provides the ability to evaluate dispersion at a variety of spatial scales. To do this, the k-function calculates the ratio of the number of points(N) within a certain distance (d) of a randomly selected point to the density of points in the entire study area (lambda). The equation for this calculation is:

$$
K(d) = \lambda^{-1}E(N_d)
$$

This process is then iterated many times using different distances. Conceptually, this process is similar to zooming in and out of an interactive map and assessing the clustering/dispersion at a large variety of scales (or distances, in the case of this analysis). The results of this iteration can then be graphed and compared against what would be expected if the points were randomly distributed across the study area; these values of k for a random distribution at distance d are given by:

$$
K_{CSR}(d) = \pi d^2
$$

When the results from the complete spatial randomness calculation are graphed, their associated confidence limits are also graphed, thereby allowing for the determination of statistical significance. Sample population k function values for a given distance that are higher than their associated values expected for a random distribution indicate clustering, while values lower than the expected (random) value indicate dispersion.

## Results

## Are bicycle theft and mischief crimes randomly located in Vancouver in 2020?

### Bicycle theft
This section of the report will outline the results of the the three statistical tests described above for bicycle theft crimes in Vancouver in 2020. The results from the nearest neighbour analysis for this crime type are described in Table 1. All values for this analysis are expressed in meters.

```{r Crime 1 NND, echo=FALSE, eval=TRUE, warning=FALSE}
#####
##Nearest Neighbour Distance
###NEAREST NEIGHBOUR
nearestNeighbour <- nndist(kma1.ppp)

##Convert the nearestNeighbor object into a dataframe.
nearestNeighbour=as.data.frame(as.numeric(nearestNeighbour))
##Change the column name to "Distance"
colnames(nearestNeighbour) = "Distance"


##Calculate the nearest neighbor statistic to test for a random spatial distribution.
#mean nearest neighbour
nnd = sum(nearestNeighbour$Distance)/nrow(nearestNeighbour)
nnd = 0.1  
#mean nearest neighbour for random spatial distribution
studyArea <- gArea(VanCity)
pointDensity <- nrow(nearestNeighbour) / studyArea
r.nnd = 1 / (2 * sqrt(pointDensity))
d.nnd = 1.07453 / sqrt(pointDensity)
R = nnd / r.nnd

SE.NND <- .26136 / sqrt(nrow(nearestNeighbour) * pointDensity)

z = (nnd - r.nnd) / SE.NND

nndResults <- data.frame(StudyArea = round(studyArea, 2),
                         NNDd = round(d.nnd, 2), 
                         NNDr = round(r.nnd, 2), 
                         NND = round(nnd, 2), 
                         Zscore = round(z, 2), 
                         Ratio = round(R, 2))

kable(nndResults, caption = "NND results for 'theft of bicycle' crimes.")
```

When comparing the NND value for this test to the values for NNDd, and NNDr, it can be observed that bicycle theft crimes in Vancouver in 2020 show a clustered pattern. The z-value of `r nndResults$Zscore` confirms that, according to the nearest neighbour method, there is strong evidence that bicycle theft crimes that have occurred in Vancouver in 2020 are significantly clustered.

Table 2 (below) provides a summary of the results from the quadrat analysis for bicycle thefts.

```{r Crime 1 Quadrat, echo=FALSE, eval=TRUE, warning=FALSE}
#####
##QUADRAT ANALYSIS
##First, determine the number of qusdrats 
quads <- 72
  
qcount <- quadratcount(kma1.ppp, nx = quads, ny = quads)

# plot(kma1.ppp, pch = "+", cex = 0.5)
# plot(qcount, add = T, col = "red")

qcount.df <- as.data.frame(qcount)

##Second, count the number of quadrats with a distinct number of points.
qcount.df <- plyr::count(qcount.df,'Freq')

##Change the column names so that x=number of points and f=frequency of quadrats with x point.
colnames(qcount.df) <- c("x","f")


sum.f.x2 <- sum(qcount.df$f * (qcount.df$x^2))

M <- sum(qcount.df$f)

N <- sum(qcount.df$x * qcount.df$f)

sum.fx.2 <- (sum(qcount.df$x * qcount.df$f)) ^ 2


VAR <- ((sum.f.x2) - (sum.fx.2 / M)) / (M - 1)

MEAN <- N/M

VMR <- VAR / MEAN
  
  
##Finally, perform the test statistic to test for the existence of a random spatial pattern.
chi.square = VMR * (M - 1)
p = 1 - pchisq(chi.square, (M - 1))

quadResults <- data.frame(Quadrats = quads * quads, 
                         Variance = round(VAR, 2), 
                         Mean = round(MEAN, 2), 
                         VMR = round(VMR, 2), 
                         Chisquare = round(chi.square, 2))

kable(quadResults, caption = "Quadrat analysis results for 'theft of bicycle' crimes.")
```


These results, particularly the relatively high variance-to-mean-ratio (VMR) indicate that these crimes are not randomly dispersed, but rather have a clustered distribution. The chi square value of `r quadResults$Chisquare` was used to calculate a p-value < 0.0001, which, when indicates strong evidence of significant clustering at a 95% confidence level.

The final test of dispersion for these bicycle theft crimes was the k-function test, for which the results are displayed below.


```{r Crime 1 K-Function, echo=FALSE, eval=TRUE, warning=FALSE, fig.cap="Ripley's k-function results for theft of bike crimes in 2020."}
#####
##K-FUNCTION 
#basic k-function
k.fun <- Kest(kma1.ppp, correction = "Ripley")
# plot(k.fun)

#use simulation to test the point pattern against CSR
k.fun.e <- envelope(kma1.ppp, Kest, nsim = 99, correction = "Ripley", verbose = FALSE)
plot(k.fun.e, main = "")
```

Because the sample population k function values (black line) are greater than the k function values representing a random distribution of these same data (red line) at distances > 100m of d, it can be said that there is strong evidence of significant clustering in these data at distances beyond 100m.

### Mischief crimes
The results for the nearest neighbour analysis for this crime type are shown in the table below.

```{r Crime 2 NND, echo=FALSE, eval=TRUE, warning=FALSE}
#####
##Nearest Neighbour Distance
###NEAREST NEIGHBOUR
nearestNeighbour <- nndist(kma2.ppp)

##Convert the nearestNeighbor object into a dataframe.
nearestNeighbour=as.data.frame(as.numeric(nearestNeighbour))
##Change the column name to "Distance"
colnames(nearestNeighbour) = "Distance"


##Calculate the nearest neighbor statistic to test for a random spatial distribution.
#mean nearest neighbour
nnd = sum(nearestNeighbour$Distance)/nrow(nearestNeighbour)
  
#mean nearest neighbour for random spatial distribution
studyArea <- gArea(VanCity)
pointDensity <- nrow(nearestNeighbour) / studyArea
r.nnd = 1 / (2 * sqrt(pointDensity))
d.nnd = 1.07453 / sqrt(pointDensity)
R = nnd / r.nnd
SE.NND <- .26136 / sqrt(nrow(nearestNeighbour) * pointDensity)
z = (nnd - r.nnd) / SE.NND

nndResults <- data.frame(StudyArea = round(studyArea, 2),
                         NNDd = round(d.nnd, 2), 
                         NNDr = round(r.nnd, 2), 
                         NND = round(nnd, 2), 
                         Zscore = round(z, 2), 
                         Ratio = round(R, 2))

kable(nndResults, caption = "NND results for 'mischief' crimes.")
```

The NND, NNDd and NNDr values from this test indicate that mischief crimes in Vancouver are spatially clustered. This result shows strong evidence of significant clustering at a 95% confidence interval as indicated by the z-score of `r nndResults$Zscore`.

The results from the quadrat analysis for mischief crimes are summarized in the following table:

```{r Crime 2 Quadrat, echo=FALSE, eval=TRUE, warning=FALSE}
#####
##QUADRAT ANALYSIS
##First, determine the number of qusdrats 
quads <- 72
  
qcount <- quadratcount(kma2.ppp, nx = quads, ny = quads)

# plot(kma1.ppp, pch = "+", cex = 0.5)
# plot(qcount, add = T, col = "red")

qcount.df <- as.data.frame(qcount)

##Second, count the number of quadrats with a distinct number of points.
qcount.df <- plyr::count(qcount.df,'Freq')

##Change the column names so that x=number of points and f=frequency of quadrats with x point.
colnames(qcount.df) <- c("x","f")


sum.f.x2 <- sum(qcount.df$f * (qcount.df$x^2))

M <- sum(qcount.df$f)

N <- sum(qcount.df$x * qcount.df$f)

sum.fx.2 <- (sum(qcount.df$x * qcount.df$f)) ^ 2


VAR <- ((sum.f.x2) - (sum.fx.2 / M)) / (M - 1)

MEAN <- N/M

VMR <- VAR / MEAN
  
  
##Finally, perform the test statistic to test for the existence of a random spatial pattern.
chi.square = VMR * (M - 1)
p = 1 - pchisq(chi.square, (M - 1))

quadResults <- data.frame(Quadrats = quads * quads, 
                         Variance = round(VAR, 2), 
                         Mean = round(MEAN, 2), 
                         VMR = round(VMR, 2), 
                         Chisquare = round(chi.square, 2))

kable(quadResults, caption = "Quadrat analysis results for 'mischief' crimes.")
```

These results indicate that these crimes have a clustered spatial distribution, with strong evidence that the clustering is statistically significant as indicated by the chi square value of `r signif(chi.square,4)`, which produces a p-value < 0.0001.

Results from the k-function test for mischief crimes are shown by the graph below.


```{r Crime 2 K-Function, echo=FALSE, eval=TRUE, warning=FALSE, fig.cap="Ripley's k-function results for mischief crimes in 2020."}
#####
##K-FUNCTION 
#basic k-function
k.fun <- Kest(kma2.ppp, correction = "Ripley")
# plot(k.fun)

#use simulation to test the point pattern against CSR
k.fun.e <- envelope(kma2.ppp, Kest, nsim = 99, correction = "Ripley", verbose = FALSE)
plot(k.fun.e, main = "")
```

The above graph indicates that mischief crimes appear to be significantly spatially clustered at distances of d greater than 75m.

## Where are the crime ‘hotspots’ located?

### Kernel Density Estimation (KDE)
In order to determine where in Vancouver the clustering of crimes described in the previous section is occurring, a kernel density estimation was performed.

Kernel density estimation is an analysis that allows for simple visualization of the spatial pattern of points in a dataset. It achieves this by creating a tessellated surface where each cell displays the density of points within a circle with a certain radius (r) placed around that cell. This circle essentially acts as the “search area”, within which points are counted and subsequently divided by the area of the circle to get a measure of density. This relationship is given by the following formula:

$$
\hat\lambda_p = \frac{no. [S \in C(\boldsymbol{p, r})]}{\pi r^2}
$$
The radius of the circle described above is the main parameter for the kernel density estimator and is formally referred to as sigma. To demonstrate the effect of changing the sigma value, we can observe the following figure:

```{r KDE SigmaChange crime 1, echo=FALSE, eval=TRUE, warning=FALSE, fig.cap="Kernel density estimations for theft of bicycle crimes in 2020 with 100m cell resolution."}
kde.100 <- density(kma1.ppp, sigma = 100, at = "pixels", eps = c(100, 100))
kde.SG <- as(kde.100, "SpatialGridDataFrame")
kde.500 <- density(kma1.ppp, sigma = 500, at = "pixels", eps = c(100, 100))
kde.SG <- cbind(kde.SG, as(kde.500, "SpatialGridDataFrame"))
kde.1k <- density(kma1.ppp, sigma = 1000, at = "pixels", eps = c(100, 100)) 
kde.SG <- cbind(kde.SG, as(kde.1k, "SpatialGridDataFrame"))
kde.5k <- density(kma1.ppp, sigma = 5000, at = "pixels", eps = c(100, 100))
kde.SG <- cbind(kde.SG, as(kde.5k, "SpatialGridDataFrame"))

  
names(kde.SG) <- c("sigma.100m", "sigma.500m", "sigma.1km", "sigma.5km")
#plot
# x11() #opens a new plot window
spplot(kde.SG)

#can see how the bandwidth selection influences the density estimates
# summary(kde.SG)

```

As shown by Figure 4, changing the sigma dramatically alters the output surface, thereby changing how the results are interpreted; this makes it important that an appropriate sigma value be chosen when conducting a kernel density estimation. One way of ensuring that one’s analysis is using the optimal sigma is through the use of a cross validation simulation, which tests a large variety of sigma values on the dataset in question and determines the value that results in the least error. This cross validation technique was used for determining sigma for both kernel density estimations performed in this analysis.

Also of importance when conducting a kernel density estimation is the cell size. One must ensure that the size of cells on the kernel density surface is appropriate for the spatial scale at which the data were recorded and/or are being studied. For the current analysis, a cell size of 100m by 100m was chosen, as it allows for a detailed enough display of crime hotspots that they can be identified at the street level, but not too much detail that redundant patterns in the data start to appear.

```{r bwd optimal Crime 1, echo=FALSE, eval=TRUE, warning=FALSE, fig.cap="KDE surface for theft of bicycle crimes in Vancouver in 2020."}
#use cross-validation to get the bandwidth that minimizes MSE
bw.d1 <- bw.diggle(kma1.ppp)
```


### KDE Results
### Bicycle theft

The optimal sigma produced by the cross reference simulation for the bicycle theft kernel density estimation was `r round(bw.d1[1],2)`m.  Using this value of sigma and a cell size of 100m by 100m produced the following kernel density estimation surface:

```{r KDE optimal Crime 1, echo=FALSE, eval=TRUE, warning=FALSE, fig.cap="KDE surface for theft of bicycle crimes in Vancouver in 2020."}
#density using the cross-validation bandwidth
kde.bwo1 <- density(kma1.ppp, sigma = bw.d1, at = "pixels", eps = c(100, 100))
plot(kde.bwo1, main = "")
```
```{r bwd optimal Crime 2, echo=FALSE, eval=TRUE, warning=FALSE, fig.cap="KDE surface for mischief crimes in Vancouver in 2020."}
#use cross-validation to get the bandwidth that minimizes MSE
bw.d2 <- bw.diggle(kma2.ppp)
```

### Mischief crimes
The optimal sigma value for the mischief crimes was determined to be `r round(bw.d2[1],2)`m, and the results from the kernel density estimation surface using this sigma value are as follows:

```{r KDE optimal Crime 2, echo=FALSE, eval=TRUE, warning=FALSE, fig.cap="KDE surface for mischief crimes in Vancouver in 2020."}
#density using the cross-validation bandwidth
kde.bwo2 <- density(kma2.ppp, sigma = bw.d2, at = "pixels", eps = c(100, 100))
plot(kde.bwo2, main = "")
```

## Conclusions

The primary objective of this study was to determine the nature of the spatial distribution of bicycle theft and mischief crimes in Vancouver in 2020. The results of the spatial analyses conducted for this study were conclusive, indicating strong evidence for that both bicycle theft and mischief crimes were spatially clustered in Vancouver in 2020. Nearest neighbour, k-function and quadrat analyses all produced results indicating that these types of crime showed statistically significant clustering, and the results from the KDE test allowed for the approximation of where these clusters were occurring. Overall, it was found that the ‘Downtown’ neighbourhood, or more generally, the central business district, experienced the highest density of these types of crimes, particularly for mischief crimes.  This is hypothesized to be a result of the concentration businesses and commercial buildings and entertainment establishments in the downtown core of Vancouver. The bicycle theft crimes showed less distinct clustering than mischief crimes (Figure 4, Figure,7), displaying more spread of crime hotspots from the downtown core.

It is hypothesized that the spatial patterns observed in this study are a result of first-order processes, whereby the location of one phenomenon (bicycle theft and mischief crimes) is influenced or driven by underlying properties of the landscape. In the urban environment, these properties are often found to be related to economic dynamics such as unemployment [3], as well as population density and demographic factors [2]. Determining the particular forces driving this clustering is beyond the scope of this report, however it is suggested that future studies investigate the spatial correlation of these types of crime in Vancouver and income, or population density.

## Limitations
The study presented above provides useful insight into the spatial distribution of bicycle theft and mischief crimes in Vancouver in 2020, however it is not without limitations.  Firstly, the positions of the spatial data are adjusted for privacy concerns, limiting the spatial accuracy of the data and increasing the chance of having duplicate points in high crime areas.  Additionally, the analyses conducted did not control for population size, making the results subject to inflation simply as a result of increased population density.

Because this study was simply aimed at determining how bicycle theft and mischief crimes in Vancouver are distributed across space, these limitations were not able to be addressed in the current analysis; however, they may be addressed by future studies on this topic. As mentioned previously, it is suggested that future studies examine in detail the hypothesized first order processes influencing the location of bicycle theft and mischief crimes in Vancouver. It is also suggested that the results of this study would benefit from comparison with similar studies for different years, allowing for the investigation of if/how these distributions have changed as a result of the COVID-19 pandemic. These temporally focused studies may benefit from employing a similar methodology as was used in this report, but they are recommended to control for population size. The importance of this is two-fold: firstly, it allows for the calculation of a crime rate, as opposed to just a number of crimes in different areas across the city and second, it standardizes the crime observations allowing for comparison across time in an area with a rapidly expanding urban population.

## References
1. Manojlovic, D. 2020. Quarter 2, 2020 Year-to-Date Key Performance Indicators Report. Vancouver Police Department. Retrieved from https://vancouver.ca/police/policeboard/agenda/2020/0917/2009P02-Q2-2020-KPI-Report.pdf

2. Cohen, LE, & Felson, M. (1979). Social change and crime rate trends: a routine activity approach. American Sociological Review, 44(4), 588–608.

3. Andersen, M. (2006). A spatial analysis of crime in vancouver, british columbia: A synthesis of social disorganization and routine activity theory. The Canadian Geographer, 50(4), 487-502.

4. Andresen, M, Curman, A., & Linning, S. (2016;2017;). The trajectories of crime at places: Understanding the patterns of disaggregated crime types. Journal of Quantitative Criminology, 33(3), 427-449.

5. Curman, N., Andresen M., Brantingham PJ. Crime and Place: A Longitudinal Examination of Street Segment Patterns in Vancouver, BC. Journal of quantitative criminology. 2014;2015;31:127-147.

6. Vancouver Crime Data Description. n.d. Vancouver Police Department.
