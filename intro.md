There has been a lot of talk lately about antibody tests, to check who
has had coronavirus infection and is now immune. The idea is that if someone carries antibodies, they have previously been infected (or, in future, vaccinated) and will be immune to infection while they still make those antibodies. There is hope that these tests can help reduce lockdown events.  See more about testing [here](https://www.bbc.co.uk/news/health-51943612).

Note, I am not a virologist, I cannot say whether these tests are
'good' or 'bad'.  But I am a statistician, so when people are talking
about these tests having "good sensitivity and specificity", but not
being accurate, I do understand how that can happen.  Inspired by
various threads talking about Bayes' theorem, and in particular,
[this
thread](https://twitter.com/hwitteman/status/1249917008935882753?ref_src=twsrc%5Etfw)
from Holly Witteman, which presents Bayes rule as a decision tree, I
decided to make a tool so I could examine how 'accurate' a test
would be in different circumstances.

To use it, we first need to consider what the concepts of accuracy, sensitivity and specificity are.

## Sensitivity and specificity
The standard way these
tests are assessed is to take samples with known status (immune or not
immune), and see how good the tests are at detecting the truth.
These are measured by two numbers:
  * sensitivity: the chance of a positive result when a sample from a truly immune sample is tested
  * specificity: the chance of a negative result when a sample from a truly non-immune sample is tested

These numbers are useful, because they are a property of the test only, but they don't tell us testing accuracy.


## Accuracy, measured by predictive values

If we are looking at a test result for ourselves, or for a population, we don't want to know sensitivity or specificity.  We care about "if a test gives a positive (negative) result, what is the chance someone is immune (not immune)"?  These are called the
  * positive predictive value: the chance of a sample being from an immune individual when a positive result is seen
  * negative predictive value: the chance of a sample being from a truly non-immune individual when a negative result is seen

Note that these are the other way round to sensitivity and specificity, which say given that someone is immune (not immune), what is the chance their test is positive (negative)?

## A picture is worth a thousand words
These are unfamiliar concepts for most people.  While most pictures are *not* worth a thousand words, I do think they can help with understanding, especially when they are interactive.  Which picture helps best will be different for everyone, and I've tried two ways, pick which you prefer:
  * a waffle plot, where every mini-square represents an individual, and the square colour the state (immune or non-immune, and test positive or test negative)
  * a tree, imagining the results for 10000 people taking a test, and seeing what number, and what percentage are immune or non-immune, and test positive or test negative.

Try adjusting the sensitivity and specificity of the test.  A good test will have high values for both of these, above 90%, but no test is perfect.

Then try seeing how this affects predictive values (accuracy), as the fraction of the people in the population who are immune changes.  You should see that it is hard to have a very accurate test when very few people are really immune.  Of course we don't know what this value is!  It will be higher in areas that have had larger outbreaks, and should increase as the pandemic continues.  Until we have vaccines, it cannot be larger than the fraction of the population that have been infected, and estimates for that range from [< 1% in Iceland](https://www.nejm.org/doi/full/10.1056/NEJMoa2006100), to [15% in Gangelt](https://www.land.nrw/sites/default/files/asset/document/zwischenergebnis_covid19_case_study_gangelt.pdf), a badly affected town in Germany.

---

Last updated 15 April 2020

Author: [Chris Wallace](https://www.mrc-bsu.cam.ac.uk/people/in-alphabetical-order/t-to-z/chris-wallace/)

---
