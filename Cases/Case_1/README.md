---
title: "LLE: Case I"
author: "Jason Orender"
date: "2022-11-06"
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
## Fit SQ
```

```
## ##------ Sat Dec 10 09:57:22 2022 ------##
```

```
## Loading required package: rlang
```

```
## Warning: package 'rlang' was built under R version 4.2.1
```

```
## Loading required package: gglasso
```

```
## Warning: package 'gglasso' was built under R version 4.2.1
```

```
## Loading required package: randomForest
```

```
## Warning: package 'randomForest' was built under R version 4.2.1
```

```
## randomForest 4.7-1.1
```

```
## Type rfNews() to see new features/changes/bug fixes.
```

```
## 
## Attaching package: 'randomForest'
```

```
## The following object is masked from 'package:dplyr':
## 
##     combine
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
## Fit LS
```

```
## ##------ Sat Dec 10 09:57:25 2022 ------##
```

```
## ##------ Sat Dec 10 09:57:38 2022 ------##
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
## Loaded glmnet 4.1-4
```

```
## 
## Attaching package: 'gridExtra'
```

```
## The following object is masked from 'package:randomForest':
## 
##     combine
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
The reason that LASSO was chosen for the discrimination algorithm is that it by
its very nature produces sparse feature sets.  For the sake of completeness,
however, here are a few alternate modeling schemes (bearing in mind that this is
the absolute simplest case submitted for review).

A decision-tree algorithm should theoretically do well with this data, but one
of the major disadvantages to these model types is overfitting to non-relevant
data and the excessive computational load required for large data sets.  This
training data set consists of 3,437 examples, which is not large and may be
considered small in some contexts, yet the fitting for Random Forest took 86
seconds, while on the same machine fitting the LASSO to the square-rectified
data (including the process of rectifying the data) took about two seconds.  The
results are superior to the standard LASSO performed on un-transformed data, but
still fall short of the LASSO performed on the transformed data.

Fitting a Random Forest model to a rectified version of the data, as might be
expected, yields a better fit.  However, the performance of the Random Forest
fit is still inferior to that of the LASSO fit on transformed data, and the
fitting process took a considerably longer 25 seconds on the same machine as the
other two runs (which puts it at ~10x the execution time of the LASSO).


```r
message("Fitting Random Forest model...")
```

```
## Fitting Random Forest model...
```

```r
timestamp()
```

```
## ##------ Sat Dec 10 09:57:39 2022 ------##
```

```r
model_rf <- modelfit(data = dset_train, fit_type = "RF", groups = dset_groups)
timestamp()
```

```
## ##------ Sat Dec 10 09:59:04 2022 ------##
```

```r
message("  Done.")
```

```
##   Done.
```

```r
message("Plotting un-transformed data examples...")
```

```
## Plotting un-transformed data examples...
```

```r
plot(model_rf, title = "Case #1 Training Set (Random Forest no transformation)", h=.5,
     ylab = "Predicted Probability", cx = hpos)
```

![](Case_1_markdown_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
plot(model_rf, title = "Case #1 Test Set (Random Forest no transformation)",
     ylab = "Predicted Probability", data = dset_test, h=.5, cx = hpos)
```

![](Case_1_markdown_files/figure-html/unnamed-chunk-1-2.png)<!-- -->

```r
message("  Done.")
```

```
##   Done.
```

```r
timestamp()
```

```
## ##------ Sat Dec 10 09:59:04 2022 ------##
```

```r
dset_train_SQ <- rectify(dset_train, groups = dset_groups)
dset_test_SQ  <- rectify(dset_test, groups = dset_groups,
                         limits = dset_train_SQ$limits)
model_rf <- modelfit(data = dset_train_SQ$data, fit_type = "RF", groups = dset_groups)
timestamp()
```

```
## ##------ Sat Dec 10 09:59:27 2022 ------##
```

```r
message("  Done.")
```

```
##   Done.
```

```r
message("Plotting transformed data examples...")
```

```
## Plotting transformed data examples...
```

```r
plot(model_rf, title = "Case #1 Training Set (Random Forest with transformation)",
     h=.5, ylab = "Predicted Probability", cx = hpos)
```

![](Case_1_markdown_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
plot(model_rf, title = "Case #1 Test Set (Random Forest with transformation)",
     ylab = "Predicted Probability", data = dset_test_SQ$data, h=.5, cx = hpos)
```

![](Case_1_markdown_files/figure-html/unnamed-chunk-2-2.png)<!-- -->
As mentioned in the paper, the group lasso was also intended to fit categorical
variables using data sets which have features that are related or dependent.
As seen below, however, the results on this particular type of problem are
actually worse than even just a standard LASSO by a large margin.  In addition,
the fitting process itself took 107 seconds on the same hardware used to
benchmark the other methods, which is considerably longer than *any* of the
other methods.  The transformed data  results also have a peculiar and irregular
shape when compared to the other methods (fitting the transformed data took
about 49 seconds on the same hardware).


```r
message("Fitting Group LASSO model...")
```

```
## Fitting Group LASSO model...
```

```r
timestamp()
```

```
## ##------ Sat Dec 10 09:59:28 2022 ------##
```

```r
model_gl <- modelfit(data = dset_train, fit_type = "GL", groups = dset_groups)
timestamp()
```

```
## ##------ Sat Dec 10 10:01:12 2022 ------##
```

```r
message("  Done.")
```

```
##   Done.
```

```r
message("Plotting un-transformed data examples...")
```

```
## Plotting un-transformed data examples...
```

```r
plot(model_gl, title = "Case #1 Training Set (Group LASSO no transformation)", h=.5,
     ylab = "Predicted Probability", cx = hpos)
```

![](Case_1_markdown_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
plot(model_gl, title = "Case #1 Test Set (Group LASSO no transformation)",
     ylab = "Predicted Probability", data = dset_test, h=.5, cx = hpos)
```

![](Case_1_markdown_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

```r
timestamp()
```

```
## ##------ Sat Dec 10 10:01:12 2022 ------##
```

```r
message("  Done.")
```

```
##   Done.
```


```r
timestamp()
```

```
## ##------ Sat Dec 10 10:01:12 2022 ------##
```

```r
# rectifying again to show an accurate time estimate which includes rectification
dset_train_SQ <- rectify(dset_train, groups = dset_groups)
dset_test_SQ  <- rectify(dset_test, groups = dset_groups,
                         limits = dset_train_SQ$limits)
model_gl <- modelfit(data = dset_train_SQ$data, fit_type = "GL", groups = dset_groups)
timestamp()
```

```
## ##------ Sat Dec 10 10:01:44 2022 ------##
```

```r
message("  Done.")
```

```
##   Done.
```

```r
message("Plotting transformed data examples...")
```

```
## Plotting transformed data examples...
```

```r
plot(model_gl, title = "Case #1 Training Set (Group LASSO with transformation)", h=.5,
     ylab = "Predicted Probability", cx = hpos)
```

![](Case_1_markdown_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
plot(model_gl, title = "Case #1 Test Set (Group LASSO with transformation)",
     ylab = "Predicted Probability", data = dset_test_SQ$data, h=.5, cx = hpos)
```

![](Case_1_markdown_files/figure-html/unnamed-chunk-4-2.png)<!-- -->

Another common choice in a situation like this with a large number of highly
correlated features is the Ridge Regression method.  In practice, these methods
produce almost the same results on this data, except that it misidentified the
correct time step on one of the weaker features (a feature that only affected
the outcome of a handful of examples and was not required to fully describe any
outcomes).  The time it took was comparable as well (~2 seconds using the same
hardware as above).  In the future, it may be advantageous to use Ridge
Regression as an alternative with nearly the same fidelity since it has a
closed form solution and may be easier to parallelize for use with high
performance computation hardware. Comparing the two sets of plots, they look
very similar but there are a few differentiating details.


```r
message("Fitting Ridge Regression model...")
```

```
## Fitting Ridge Regression model...
```

```r
timestamp()
```

```
## ##------ Sat Dec 10 10:01:44 2022 ------##
```

```r
model_rr <- modelfit(data = dset_train, fit_type = "RR", groups = dset_groups)
timestamp()
```

```
## ##------ Sat Dec 10 10:01:57 2022 ------##
```

```r
message("  Done.")
```

```
##   Done.
```

```r
message("Plotting un-transformed data examples...")
```

```
## Plotting un-transformed data examples...
```

```r
plot(model_rr, title = "Case #1 Training Set (Ridge Regression no transformation)",
     h=.5, ylab = "Predicted Probability", cx = hpos)
```

![](Case_1_markdown_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
plot(model_rr, title = "Case #1 Test Set (Ridge Regression no transformation)",
     data = dset_test, h=.5, cx = hpos, ylab = "Predicted Probability")
```

![](Case_1_markdown_files/figure-html/unnamed-chunk-5-2.png)<!-- -->

```r
message("  Done.")
```

```
##   Done.
```

```r
timestamp()
```

```
## ##------ Sat Dec 10 10:01:57 2022 ------##
```

```r
dset_train_SQ <- rectify(dset_train, groups = dset_groups)
dset_test_SQ  <- rectify(dset_test, groups = dset_groups,
                         limits = dset_train_SQ$limits)
model_rr <- modelfit(data = dset_train_SQ$data, fit_type = "RR", groups = dset_groups)
timestamp()
```

```
## ##------ Sat Dec 10 10:01:59 2022 ------##
```

```r
message("  Done.")
```

```
##   Done.
```

```r
message("Plotting transformed data examples...")
```

```
## Plotting transformed data examples...
```

```r
plot(model_rr, title = "Case #1 Training Set (Ridge Regression with transformation)",
     h=.5, ylab = "Predicted Probability", cx = hpos)
```

![](Case_1_markdown_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
plot(model_rr, title = "Case #1 Test Set (Ridge Regression with transformation)",
     data = dset_test_SQ$data, h=.5, cx = hpos, ylab = "Predicted Probability")
```

![](Case_1_markdown_files/figure-html/unnamed-chunk-6-2.png)<!-- -->

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

