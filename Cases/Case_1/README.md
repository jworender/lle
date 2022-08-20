---
title: "LLE: Case I"
author: "Jason Orender"
date: "2022-08-20"
output: 
  html_document: 
    keep_md: yes
---



## Generate the Data

The data generation script is in the file named "build_case_1_data.R".  It will
generate three files: 1) dset, 2) dset_train, 3) dset_test.  The "dset" file is
a consolidated data set, and "dset_train" and "dset_test" are mutually exclusive
subsets of "dset".  This document can be run it as a notebook, bock by block, 
or it can also be knitted into an html document if desired.

The "relevant" data is taken from curves 5, 8, 9, 10, 13, 25, and 30.  They are
marked "RELEVANT" in the chart title.  The plots generated from the next code
block show 40 different curves, only 7 of which contain relevant data.  The
others are added to obfuscate the data and make the task of picking out the
relevant features and time steps harder.  The red markers on the curves show
the time steps for which all of the criteria for an event are met.  Each time
step generates an example which includes the data from the ten time steps before.

![](Case_1_markdown_files/figure-html/generate_data-1.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-2.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-3.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-4.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-5.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-6.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-7.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-8.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-9.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-10.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-11.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-12.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-13.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-14.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-15.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-16.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-17.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-18.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-19.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-20.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-21.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-22.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-23.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-24.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-25.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-26.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-27.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-28.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-29.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-30.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-31.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-32.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-33.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-34.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-35.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-36.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-37.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-38.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-39.png)<!-- -->![](Case_1_markdown_files/figure-html/generate_data-40.png)<!-- -->

## The data generation process

The following process is common to all of the synthetic data sets.

The simulated data was generated with the following requirements in mind:

1.	The data needed to be time-varying.  Each independent variable had a
    continuous stream of data that varied over time.  This was necessary to
    generate realistic scenarios that show the potential for analyzing lagged
    longitudinal data.
2.	The data needed to be range bound.  This reflected the desire to model
    realistic physical attributes, which tend to neither rise nor fall without
    end.
3.	The data needed to be complex.  In order to provide a convincing test, it
    needed to have no easily discernable pattern.
4.	The data needed to be cyclic.  Since the idea was to simulate sensor data or
    physical processes, and enough interactions needed to be generated in order
    to create a robust data set, some sort of cyclic process was necessary.

To meet these requirements, a sine function was chosen as the basic building
block for the simulated data.  Some additional details:

1.	When necessary, the Mersenne-Twister pseudorandom algorithm was used to
    generate pseudorandom numbers.  This is the default algorithm when
    generating pseudorandom numbers via the base R package.
2.	To generate complexity, multiple sine waves of varying frequency and
    amplitude were combined via superposition.
3.	The data was generated first, and then particular data streams were chosen
    and designated as the “relevant” features.
    
The procedure (the code is in the "build_case_1_data.R" script file):

1.	Generate a large number of sine waves with five possible randomly chosen
    frequencies per cycle.  Something like this was generated:  
    ![Curve Generation Step #1](../../Paper/Graphics/curve_generation_1.png)
2.  Superpose several of the curves generated in step 1.  Something like the
    curve next curve was generated by this step:  
    ![Curve Generation Step #2](../../Paper/Graphics/curve_generation_2.png)
3.	To create a diversity of values a new range was selected using a
    pseudorandom normal distribution centered on zero, standard deviation of
    0.05, and multiplied by 100. This produced a large enough diversity of
    magnitudes to demonstrate the range agnostic quality of the method.
4.	A version of the waves was shifted by a pre-determined amount to simulate
    lag.  The next graphic shows an example of this. Using lagged data simulates
    sensor data with readings that are time-late by an unknown amount, or an
    effect that has an unknown delay time between the change in reading and the
    occurrence of an event.   
    ![Curve Generation Step #3](../../Paper/Graphics/curve_generation_3.png)
5.	Assemble enough curves to have a large selection of potentially “relevant”
    and non-relevant curves.  Fig. 9 shows how the relevant curves are
    interpreted to generate events.  Note that the shifted curves determine when
    the event occurs, while the unshifted curves are matched with the time step
    in which they occurred to compose the data.  
    ![Curve Generation logical AND illustration](../../Paper/Graphics/curve_logical_AND.png)
6.	Each time step is interpreted as a separate example, and if all relevant
    curves meet the selection criteria during that time step, it is labeled as a
    “True” example, and otherwise is labeled as “False”.  The data for each
    example is taken from the unshifted curves.
7.	The data is next flattened such that a row contains the data from the
    current time step as well as the ten time steps previous (this is adjustable
    in the script).  The data from time steps zero (the current time step)
    through nine (the data nine time steps in the past) on the row for one
    example, for instance, will be the data for time steps one through ten on
    the next chronological example.  The next graphic illustrates:
    ![Curve Generation flatten](../../Paper/Graphics/curve_flatten.png)
8.	The final step is to randomize the rows to prevent any relationship between
    time steps from influencing the final outcome of the experiment.  There is
    no indication in the flattened data features where an example belongs
    chronologically with respect to the other examples.
9.	Once the data set has been created, it is split into training and testing
    sets with 70% of the data used for training and 30% of the data used for
    testing.

The procedure described generated a data set that included several relevant
features combined via a logical AND, is representative of what might be observed
with real sensor measurements and is complex enough to prevent any latent
patterns within the data from influencing the results.  The last example will
also illustrate what happens when a logical OR is introduced.

A data set with relevant features combined via a logical OR or a combination of
logical OR and logical AND will be generated in an identical way up to step 6.
Step 6 will be modified accordingly to generate the desired relationships among
the relevant variables.





```r
message("Detecting groups...")
```

```
## Detecting groups...
```

```r
dstruct     <- organize(dset_train)
dset_groups <- dstruct$groups
message("  Done.")
```

```
##   Done.
```

```r
# This code block uses a specialized routine called "modelfit" which is a
# generalized routine that fits data to a specified model type.  It was made
# to simplify the interface and make the code more succinct.
message("Fitting Models...")
```

```
## Fitting Models...
```

```r
# SQ means square rectified, which is a LASSO run on transformed data
model_1 <- modelfit(data = dset_train, dtype = "SQ", groups = dset_groups,
                params = list(sdfilter = NULL))
```

```
## Warning: package 'rlang' was built under R version 4.2.1
```

```
## Loaded glmnet 4.1-4
```

```
## Loading required package: openssl
```

```
## Warning: package 'openssl' was built under R version 4.2.1
```

```
## Linking to: OpenSSL 1.1.1k  25 Mar 2021
```

```r
# LS just means LASSO
model_2 <- modelfit(data = dset_train, dtype = "LS", groups = dset_groups)
message("  Done.")
```

```
##   Done.
```

```r
message("Plotting examples...")
```

```
## Plotting examples...
```

```r
plot(model_1, title = "Case #1 Training Set", h=.5)
```

```
## 
## Attaching package: 'gridExtra'
```

```
## The following object is masked from 'package:dplyr':
## 
##     combine
```

![](Case_1_markdown_files/figure-html/plots-1.png)<!-- -->

```r
plot(model_1, title = "Case #1 Test Set", data = dset_test, h=.5)
```

![](Case_1_markdown_files/figure-html/plots-2.png)<!-- -->

```r
plot(model_2, title = "Case #1 Training Set (no transformation)", h=.5)
```

![](Case_1_markdown_files/figure-html/plots-3.png)<!-- -->

```r
plot(model_2, title = "Case #1 Test Set (no transformation)",
     data = dset_test, h=.5)
```

![](Case_1_markdown_files/figure-html/plots-4.png)<!-- -->

```r
message("  Done.")
```

```
##   Done.
```

```r
TRANS   <- 0.2
BARHT   <- 7.5
beta1   <- model_1$model$beta[,]
markers <- rep(0,length(beta1))
cols    <- rep(rgb(0,0,0,0), length(beta1))

cols[grep("V5TM10", names(beta1))]     <- rgb(1.0, 0.0, 0.0,TRANS) #RED
markers[grep("V5TM10", names(beta1))]  <- BARHT
cols[grep("V8TM10", names(beta1))]     <- rgb(0.5, 0.0, 0.5,TRANS) #PURPLE
markers[grep("V8TM10", names(beta1))]  <- BARHT
cols[grep("V9TM1", names(beta1))]      <- rgb(0.0, 1.0, 0.0,TRANS) #GREEN
markers[grep("V9TM1", names(beta1))]   <- BARHT
cols[grep("V10TM0", names(beta1))]     <- rgb(1.0, 0.5, 0.0,TRANS) #ORANGE
markers[grep("V10TM0", names(beta1))]  <- BARHT
cols[grep("V13TM2", names(beta1))]     <- rgb(1.0, 0.0, 1.0,TRANS) #MAGENTA
markers[grep("V13TM2", names(beta1))]  <- BARHT
cols[grep("V25TM3", names(beta1))]     <- rgb(0.1, 0.5, 0.5,TRANS) #TEAL
markers[grep("V25TM3", names(beta1))]  <- BARHT
cols[grep("V30TM7", names(beta1))]     <- rgb(0.1, 0.1, 0.0,TRANS) #DARK BROWN
markers[grep("V30TM7", names(beta1))]  <- BARHT

par(new=FALSE, mar = c(6,3,3,2))
barplot(beta1, border = "blue", col = "blue", ylim = c(-0.5, BARHT), las = 2,
        xaxt = "n")

par(new=TRUE)
barplot(markers, border = cols, col = cols, ylim = c(-0.5, BARHT),
        axes = FALSE)
```

![](Case_1_markdown_files/figure-html/plots-5.png)<!-- -->

```r
BARHT   <- 30
beta2   <- model_2$model$beta[,]
markers <- rep(0,length(beta2))
cols    <- rep(rgb(0,0,0,0), length(beta2))
  
cols[grep("V5TM10", names(beta2))]     <- rgb(1.0, 0.0, 0.0,TRANS) #RED
markers[grep("V5TM10", names(beta2))]  <- BARHT
cols[grep("V8TM10", names(beta2))]     <- rgb(0.5, 0.0, 0.5,TRANS) #PURPLE
markers[grep("V8TM10", names(beta2))]  <- BARHT
cols[grep("V9TM1", names(beta2))]      <- rgb(0.0, 1.0, 0.0,TRANS) #GREEN
markers[grep("V9TM1", names(beta2))]   <- BARHT
cols[grep("V10TM0", names(beta2))]     <- rgb(1.0, 0.5, 0.0,TRANS) #ORANGE
markers[grep("V10TM0", names(beta2))]  <- BARHT
cols[grep("V13TM2", names(beta2))]     <- rgb(1.0, 0.0, 1.0,TRANS) #MAGENTA
markers[grep("V13TM2", names(beta2))]  <- BARHT
cols[grep("V25TM3", names(beta2))]     <- rgb(0.1, 0.5, 0.5,TRANS) #TEAL
markers[grep("V25TM3", names(beta2))]  <- BARHT
cols[grep("V30TM7", names(beta2))]     <- rgb(0.1, 0.1, 0.0,TRANS) #DARK BROWN
markers[grep("V30TM7", names(beta2))]  <- BARHT
  
par(new=FALSE, mar = c(6,3,3,2))
barplot(beta2, border = "blue", col = "blue", ylim = c(-30, BARHT), las = 2,
        xaxt = "n")

par(new=TRUE)
barplot(markers, border = cols, col = cols, ylim = c(-30, BARHT),
        axes = FALSE)
```

![](Case_1_markdown_files/figure-html/plots-6.png)<!-- -->

