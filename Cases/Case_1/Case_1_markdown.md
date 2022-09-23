---
title: "LLE: Case I"
author: "Jason Orender"
date: "2022-08-28"
output: 
  html_document: 
    keep_md: yes
---


## Introduction

This document was created by running the "Case_1_markdown.Rmd" file in RStudio.
It utilizes the script files "functions.R" and "rectify.R" within the "Code"
directory in this repository, which are used to define the functions that are
called below.  This document will create all of the synthetic data, fit the data
to a model, and then display the results.  Anyone interested is invited to clone
or fork the repository and change the settings for the purposes of 
self-demonstration. The verbiage here reflects the text in the paper, but in
some cases figures or examples that are present here were cut out of that
document in order to stay within a ten page limit.

## Generate the Data

The data generation script is in the file named "build_case_1_data.R".  It will
generate three data sets: 1) dset, 2) dset_train, and 3) dset_test.  The "dset"
object is a consolidated data set, and "dset_train" and "dset_test" objects are
mutually exclusive subsets of "dset".  This document can be run as a notebook,
block by block, or it can also be knitted into an html document if desired.

The "relevant" data is taken from curves 5, 8, 9, 10, 13, 25, and 30.  They are
marked "RELEVANT" in the chart title.  The plots generated from the next code
block show 40 different curves, only 7 of which contain relevant data.  The
others are added to obfuscate the data and make the task of picking out the
relevant features and time steps harder.  The red markers on the curves show
the time steps for which all of the criteria for an event are met.  Each time
step generates an example which includes the data from the ten time steps before.

The data generation script will produce a plot of the entire data set for each
feature over each time step.  There are a large number of these plots, but the
process for generation and the explanation of the results is located below.

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
    ![Curve Generation Step #1](../../Papers/Graphics/curve_generation_1.png)
2.  Superpose several of the curves generated in step 1.  Something like the
    curve next curve was generated by this step:  
    ![Curve Generation Step #2](../../Papers/Graphics/curve_generation_2.png)
3.	To create a diversity of values a new range was selected using a
    pseudorandom normal distribution centered on zero, standard deviation of
    0.05, and multiplied by 100. This produced a large enough diversity of
    magnitudes to demonstrate the range agnostic quality of the method.
4.	A version of the waves was shifted by a pre-determined amount to simulate
    lag.  The next graphic shows an example of this. Using lagged data simulates
    sensor data with readings that are time-late by an unknown amount, or an
    effect that has an unknown delay time between the change in reading and the
    occurrence of an event.   
    ![Curve Generation Step #3](../../Papers/Graphics/curve_generation_3.png)
5.	Assemble enough curves to have a large selection of potentially “relevant”
    and non-relevant curves.  Fig. 9 shows how the relevant curves are
    interpreted to generate events.  Note that the shifted curves determine when
    the event occurs, while the unshifted curves are matched with the time step
    in which they occurred to compose the data.  
    ![Curve Generation logical AND illustration](../../Papers/Graphics/curve_logical_AND.png)
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
    ![Curve Generation flatten](../../Papers/Graphics/curve_flatten.png)
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



## Plot the Results

The first case is a longitudinal data problem in which the following specific characteristics apply:

1.	7 relevant variables are intermixed with 33 non-relevant variables, all 7 of which must be within their relevant ranges to trigger an event.
2.	Each variable is tracked over 10 previous time-steps as well as the current time step (time step zero) and as a consequence generates 11 features per variable, for a total of 440 features per example in the flattened data.
3.	There is a single time-step/feature relevant for each variable.  As a result, the ratio of non-relevant to relevant features is approximately 63:1.  The time step features are meant to describe the delayed effect once the variable enters its critical range.
4.	There are 3437 examples in the training set, 530 of which are positive examples.
5.	There are 1474 examples in the test set, 232 of which are positive examples.  All test set examples are unseen until tested against the trained model and should be considered novel data.



```
## Detecting groups...
```

```
##   Done.
```

```
## Fitting Models...
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

```
##   Done.
```
The next plots, which show the results of the model fit plotted against the
training and then the test set, tells a very straightforward story.  The
resultant model describes the data extremely well when the data is transformed
into a binary format using the previous procedure.  


```
## Plotting transformed data examples...
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

![](Case_1_markdown_files/figure-html/transformed_data_plots-1.png)<!-- -->![](Case_1_markdown_files/figure-html/transformed_data_plots-2.png)<!-- -->

```
##   Done.
```

If the transformation is not used, the next plot shows the resultant LASSO fit
on the un-transformed data.  Without the transformation, the LASSO fit severely
underperforms with respect to the fit performed with the transformed data. In
addition, the computational power required to calculate the fit on the un-
transformed data is significantly greater.  The LASSO fit on the un-transformed
data took nearly double (1.83x) the amount of time required to both transform
the data and then perform the fit in the transformed data case
(single-threaded). Once the data is transformed, the remainder of the
calculations are nearly all integer math until the final coefficients are
calculated. This could prove a significant advantage when applied in a high
performance computing (HPC) setting.


```
## Plotting un-transformed data examples...
```

![](Case_1_markdown_files/figure-html/untransformed_data_plots-1.png)<!-- -->![](Case_1_markdown_files/figure-html/untransformed_data_plots-2.png)<!-- -->

```
##   Done.
```

The advantages of the transformation procedure go beyond the accuracy of the
results and the reduced computational power.  The precision with which it can
pinpoint the precise features required for the sparse solution means that in a
longitudinal data scenario such as this, both the variable and the specific lag
for each variable can potentially be derived.  The bar plot below shows the
coefficients calculated for the LASSO applied to the transformed data.  The
sparseness of the solution is almost ideal; there are only five spurious returns
and three of those were directly adjacent to the correct return.

![](Case_1_markdown_files/figure-html/transformed_data_barplot-1.png)<!-- -->

This can be contrasted with the LASSO solution on the un-transformed data in
the following plot.  The scattershot arrangement of the coefficient magnitudes
clearly reveals why the solution shown above for the un-transformed data so
greatly underperforms.

![](Case_1_markdown_files/figure-html/untransformed_data_barplot-1.png)<!-- -->

