# LLE: The LASSO Logic Engine
## (harnessing the logic parsing capabilities of the LASSO algorithm for longitudinal feature learning)

#### Authors
1. Jason Orender (<a href = "mailto:joren001@odu.edu">joren001@odu.edu</a>)
2. Mohammad Zubair (<a href = "mailto:zubair@cs.odu.edu">zubair@cs.odu.edu</a>)
3. Jiangwen Sun (<a href = "jsun@cs.odu.edu">jsun@cs.odu.edu</a>)

### Abstract  
Longitudinal data, which is widely used in many disciplines to study cause and
effect, poses significant computational challenges to both modeling and
analysis.  Longitudinal data is composed of readings on the same variable
collected over time and is often high-dimensional with correlated features.  The
combinatorial search approach for identifying the optimal features is
unrealistic for most applications.  The alternative approaches, such as
heuristics, greedy searches, and regularization techniques, including LASSO, can
result in models that suffer from both low accuracy and unclear feature
attribution. In this paper, we propose a binary transformation on the data
before applying LASSO for feature learning.  As demonstrated in the paper, the
binary transformation enhances signal in the data, resulting in highly accurate
feature attribution, including associated time lags. It avoids the typical
shortcomings of the LASSO algorithm, including saturation of the feature space
and arbitrary or inconsistent sparse feature selection.  Both synthetic data and
real-world data sets were used to demonstrate the value of the proposed
transformation and in every case substantial improvements in feature learning
were seen.  In addition, the scalability of the solution is superior to that of
the standard LASSO since an integrated high performance computing (HPC) solution
would utilize integer math for most of the computational effort.

### Introduction

Longitudinal data refers to the observation of several independent variables
(features) along with a specific outcome of interest for a period of time. The
need for analysis of longitudinal data occurs in many disciplines, such as
medical research [<a href="https://escholarship.org/content/qt67p9d40p/qt67p9d40p.pdf">1</a>, <a href="https://www.researchgate.net/profile/L-Alan-Sroufe-2/publication/5390481_The_Construction_of_Experience_A_Longitudinal_Study_of_Representation_and_Behavior/links/5c327a83458515a4c712b2ed/The-Construction-of-Experience-A-Longitudinal-Study-of-Representation-and-Behavior.pdf">2</a>, <a href="https://www.liebertpub.com/doi/pdf/10.1089/neu.2016.4677">3</a>], business [<a href="https://scholar.google.com/scholar?hl=en&as_sdt=0%2C47&q=Evolving+sustainably%3A+A+longitudinal+study+of+corporate+sustainable+development.+Strategic+management+journal&btnG=">4</a>, <a href="https://scholar.google.com/scholar?hl=en&as_sdt=0%2C47&q=achievement+and+entrepreneurship%3A+A++longitudinal+study&btnG=">5</a>, <a href="https://www.researchgate.net/profile/Colette-Henry-2/publication/247738795_The_Effectiveness_of_Training_for_New_Business_CreationA_Longitudinal_Study/links/54ca0a200cf2807dcc288644/The-Effectiveness-of-Training-for-New-Business-CreationA-Longitudinal-Study.pdf">6</a>, <a href="https://scholar.google.com/scholar?hl=en&as_sdt=0%2C47&q=A+longitudinal+study+of++climates.+Journal+of+organizational+behavior&btnG=">7</a>, <a href="https://scholar.google.com/scholar?hl=en&as_sdt=0%2C47&q=Team++climate%2C+climate+strength+and+team+performance.+A+longitudinal+study&btnG=">8</a>], climate science [<a href="https://link.springer.com/article/10.1007/s00484-010-0396-z">9</a>], and
experimental scientific disciplines of many kinds [<a href="https://www.frontiersin.org/articles/10.3389/fgene.2019.00963/full">10</a>, <a href="https://academic.oup.com/nar/article/50/5/e27/6457960">11</a>, <a href="https://scholar.google.com/scholar?hl=en&as_sdt=0%2C47&q=Age-related++changes+in+intramuscular+and+subcutaneous+fat+content+and+fatty+acid++composition+in+growing+pigs+using+longitudinal+data&btnG=">12</a>]. Analysis of such
data is critical in developing drugs, effective treatments, and uncovering new
cause and effect relationships in the physical sciences as well as psychological
inquiry.  A way is needed for the researcher to consistently and accurately
identify not only the variables of interest in the data, which rules out many
“black box” type solutions, but any time lag interposed between cause and
effect.

This sort of data can pose a significant challenge in that it often encompasses
a large set of features with the added complexity that many of them may be
highly correlated. A large number of features (high dimensionality of the data)
makes it computationally difficult to analyze the data set and build models. In
addition, the potential correlation between many features poses a problem for
feature selection (learning) techniques in that it is difficult for one feature
to stand out among many that are somewhat similar.  Since naïve combinatorial
strategies are unrealistic for any but the smallest data sets, this data will
typically be analyzed via heuristics [13, 14, 15, 16], greedy searches [17, 18,
19], or regularization techniques [20, 21, 22, 23].  However, these methods
cannot guarantee optimality [24] and will often select a specific feature from
a set which demonstrates multicollinearity arbitrarily and inconsistently [25].
Frequently, the researcher will also attempt to solve the multicollinearity
problem by pruning the feature set using some expert knowledge specific to a
particular information domain.

*Related works*. Using LASSO for feature selection with categorical variables is
not a new concept.  The “group LASSO” as originally conceived by Yuan and Lin
[26], implemented by Meier, van de Geer and Buhlmann [27] as well as Yang and
Zou [28] who based their application on the work of Kim, et. al. [29] was
created specifically for this type of problem.  This strategy uses an additional
penalty that is assigned to groups of categorical variables which in typical
application are intended to represent different states of a single variable.
Those papers essentially improve upon the LASSO algorithm in a specific
circumstance. In contrast, this work proposes a novel application of the
algorithm somewhat abstracted from any particular implementation. In any case,
group LASSO is known to suffer from estimation inefficiency and selection
inconsistency [30].

The work of Katrutsa and Strijov [24] is similar in concept to this one.  They
propose representing a feature presence using a binary vector, but they
propose using a system based on quadratic programming in which the quadratic
term is a pairwise feature similarity and the linear term is a measure of
relevance.  Their objective was to “minimize the number similar features and
maximize the number of relevant features.”  As stated, the selection criteria in
that work are evaluated pairwise, so by utilizing information on relevance
gleaned from a series of hypothesis tests (with their associated p-values) and
judging the similarity based on either the Pearson correlation coefficient or
estimating the probability distribution using the concept of shared mutual
information, the most relevant features that were also judged to be sufficiently
dissimilar were retained.  Since all features are never evaluated
simultaneously, however, any confounding factors will potentially be missed, and
this method is also comparatively computationally intensive and may not scale
well.

*Motivation*. This methodology was developed specifically for situations in
which a confluence of events causes another event, for the simple reason that
most of the methods available have shortcomings when applied to this specific
problem type. The case studies will show that using a traditional regression
analysis can produce comparatively substandard results in situations like this,
especially when specific causal attribution is desired.  This confluence is
considered as a logical event in which a set of binary conditions ‘A’, ‘B’, ‘C’,
etc. are met in order to produce event ‘E’.  While the input conditions are
binary in the sense of whether or not a condition has occurred, the binary
element may be couched in terms of a continuous variable threshold, for example
whether a pressure or temperature reading has exceeded or dipped below a
threshold. An excellent example of this type of thresholding behavior in a
physical application is demonstrated with the brittle fracture of materials.  If
a material is below a certain temperature and the stress increases above a
specific amount, the material breaks catastrophically in brittle fashion.  The
problem being examined here comes about when such a phenomenon presents itself
and the reasons are unknown, but massive amounts of data are available.  This is
perhaps sensor data on an engine, or possibly the results of a survey intended
to identify risk factors.  Sifting that data in an efficient manner to weed out
irrelevancies has become an extremely important task in the age of big data.
The case studies and practical analyses presented later in this paper will show
some examples of these problem types as well as proposed solution methods.

<h5 align="center">TABLE I. 	TEST SET RESULTS SUMMARY</h5>  

Case Study  | Type           | Transformed (TP/FP)  | Un-Transformed (TP/FP)  |
:----------:|:--------------:|:--------------------:|:-----------------------:|
#1          | Synthetic Data | 97.8% / 0.0%         | 85.3% / 2.1%            |
#2          | Synthetic Data | 97.7% / 2.5%         | 82.8% / 2.4%            |
#3          | Synthetic Data | 99.5% / 0.0%         | 71.4% / 6.5%            |
#4          | Synthetic Data | 91.6% / 3.1%         | 47.6% / 7.1%            |
GB          | Real Data Set  | 97.3% / 11.9%        | 93.3% / 23.8%           |

The effectiveness of the method described here is demonstrated by case studies
shown in section 4 using synthetic data, and section 5 using real-world data
(see Table I).  They show that not only are the solutions consistent with known
results and that they reliably outperform the same calculation on un-transformed
data, but that they are also both repeatable and sparse. Beyond the metrics
shown in the table below, however, the precision with which it is possible to
identify contributing features and associated time lags using this method is a
valuable asset in itself.

In this paper, we propose a technique based on a targeted reduction in the input
data's information content by creating a boolean version of the data that, when
subjected to the LASSO algorithm, significantly outperforms a similar analysis
on un-transformed data.  This technique minimizes the aforementioned weaknesses,
and we can also show via experimentation that optimality is achievable.

The main contributions of this paper are:

- Consistent and optimal feature selection.
- Computational efficiency at scale.
- Addressing the collinearity problem via optimal feature selection.
- A novel approach to modeling multiple independent causes for the same event
(P(Event)=P(A∪B)).
- A demonstration of these advantages by comparison with a LASSO model using the
same (un-transformed) data.

*Organization*.  The paper is organized into six sections.  After the
Introduction, a short background section is meant to give a quick overview
regarding what LASSO is and why it works.  The next section presents the
proposed LASSO logic engine, transforming the continuous data set into a matrix
of logical vectors followed by applying LASSO for feature learning.  The next
section both introduces the approach for generating the synthetic data and
discusses the case studies that utilize that data.  The next section covers
application to a real-world data set. Finally, the conclusion summarizes the
main points and discusses the potential for application.

All case studies, synthetic data, real data sets, and implementation code are
available in the GitHub repository for this paper
(https://github.com/jworender/lle), and readers are invited to fork the
repository and modify the parameters for the purpose of self-demonstration.

	
