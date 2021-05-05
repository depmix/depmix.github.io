---
layout: hmmr-page
title: About
category: hmmr
---

_Mixture and hidden Markov models with R_ is written by Ingmar Visser and
Maarten Speekenbrink.

The book aims to provide a self-contained practical introduction to mixture
models and hidden Markov models. The reason for introducing both
in one book is that there are very close links between these models.
This allows us to introduce important concepts, such as maximum
likelihood estimation and the Expectation-Maximization algorithm, in
the relatively simpler context of mixture models. Approaching hidden
Markov models from a thorough understanding of mixture models involves,
we hope, a relatively small conceptual leap.

We aimed to provide a reasonable
balance between statistical theory and practice. The objective is to provide
enough mathematical details to allow our target audience to
understand key results that are necessary to apply these models.
Our target audience are those with a more applied
background, in particular researchers, graduate, and advanced undergraduate
students in the social and behavioural sciences. Researchers or future
researchers hence who see the potential for applying these models and
explaining heterogeneity in their data, but who lack the tools to
fulfil this potential.

To familiarise readers with the possibilities of mixture and hidden Markov models,
a large part of the book consists of practical examples of applying
these models, many of which taken from our own research in
developmental and experimental psychology, as well as from other fields,
such as climate change and economics. These examples show how to analyse
mixture and hidden Markov models with `R`, with a particular focus on our
[`depmixS4`](https://depmix.github.io/depmixS4) package, as well as the accompanying `hmmr` package which contains
all the datasets used in the book, as well as a number of additional convenience
functions.

The book consists of seven chapters:

__Chapter 1__ provides a brief introduction to R, and describes the datasets
analysed in the examples.

__Chapter 2__ is a theoretical chapter on mixture models. It covers the
definition of these models, methods for maximum likelihood parameter estimation,
parameter inference via likelihood ratio tests and confidence intervals, model
selection techniques to e.g. determine the number of mixture components, how to
model the effect of covariates on the prior probability of the components, and
identifiability of mixture models.

__Chapter 3__ is an applied chapter with several applications of mixture and
latent class models. These include of univariate and multivariate
Gaussian mixture models for financial and
psychological data, a latent class model for multivariate binary data, and binomial
mixture models. Topics treated with these applications include local maxima in the
likelihood and other practical difficulties in model estimation, item
homogeneity, direct effects of covariates on responses vs indirect effects on
the prior probability of mixture components, and use of the bootstrapped
likelihood ratio test to determine the number of mixture components.

__Chapter 4__ is another theoretical chapter, focusing on hidden Markov models.
Building on Chapter 2, this chapter shows how hidden Markov models can be viewed
as an extension of mixture models to allow dependency between the states
(mixture components) at consecutive time points. Important properties of Markov
chains such as stationarity, homogeneity, and ergodicity, are discussed.
After moving on to hidden Markov models, inference of the hidden states via
filtering and smoothing recursions are treated in detail, as  well as state decoding
via the Viterbi algorithm. Other topics include the use of covariates to model
initial state and state transition probabilities, and dealing with missing
data.

__Chapter 5__ is an applied chapter which describes applications of hidden Markov
models (HMMs) for univariate
time series. These include a Gaussian HMM for financial time series, a Bernoulli
HMM applied to a relatively large number of relatively short timeseries, a
Gaussian HMM applied to (log) response times, a Gaussian HMM to detect
change points in climate change data, and HMMs with generalized linear models,
which can be viewed as modelling a "switching GLM". Some topics treated with
these examples include accounting for autocorrelation in timeseries, constraining
state transitions to detect change-points, and using different covariates to
predict responses in different states.

__Chapter 6__ is another applied chapter which focuses on hidden Markov
models for multivariate time series. These include a multivariate Binomial HMM
for a large number of replications of relative short timeseries, showcasing how
HMMs can be used to analyse complex panel data. Other application concerns
a HMMs for mixed data, consisting of a Bernoulli and
a Gaussian variable, and consisting of a binomial and multinomial variable.
Some of the topics treated in these applications include testing hysteresis,
testing conditional independence of responses within states, and models in
which one response variable can act as a predictor for another response variable.

__Chapter 7__ is a mostly theoretical chapter, discussing several extensions of
the material presented thus far. This includes expanding the state-space to
allow for higher-order Markov models (where the current state can depend on
states further in the past than the immediately preceding state) and models with
multiple simultaneous hidden states. The "classification likelihood" is discussed
as an alternative to the usual likelihood function, and techniques for dealing
with practical
estimation issues are also described, as well as using Bayesian estimation and
inference.


## About the authors

__Ingmar Visser__ is an Associate Professor in Developmental Psychology at the
University of Amsterdam. His core interest is in characterizing the building
blocks of the human cognitive architecture, such as the concepts we use to
classify objects in the world around us. These building blocks are shaped during
learning and development, for example in category learning and in implicit
learning. Across different experimental paradigms he develops state-of-the-art
analytical approaches to further understanding of their results. Obviously,
these methods include mixture and hidden Markov models.

__Maarten Speekenbrink__ is an Associate Professor in Mathematical Psychology at
University College London. His research focuses mostly on human learning and
decision making, adopting the formal framework of reinforcement learning to address
topics such as how uncertainty drives exploration and exploitation, and how useful
representations are formed and learned to support further learning and
generalization. He develops and applies advanced statistical methods to
investigate these topics, including Gaussian Process regression, particle
filters, and of course mixture and hidden Markov models. He teaches advanced
statistics to postgraduate students, and has written several tutorials on the
aforementioned techniques.

The authors have known each other since their days as PhD students at the
department of Psychology at the University of Amsterdam, and have remained
friends ever since. Large parts of the book were written during intensive stays
in Amsterdam, London, and Berlin, dispersed by long hiatuses where other
priorities took over.
