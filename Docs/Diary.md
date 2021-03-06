Amazon project
========================================================
### Iteration 1  -  Submitted

* Lets try to reproduce the benchmark for k nearest neighbors with knn
* I did it straight on the data without any data transformations, but I think it makes no sense for categorical data! The distance measured between features makes no sense. 
* I also did the cross validation
* Lets submit it it anyway!
* Not bad! just below knn benchmark
* 1081 at leader board :)

*Leave on out cross validation:* 0.91

*Submission score:* 0.63083 


========================================================
### Iteration 2  - Submitted

* Submitted all 1s! As all requests accepted!
* 1274th place in leader board :)
* As a benchmark, the k-nearest neighbors produces 0.63 with k=1!
* This is a result expected for random class assignment as measured by AUC

*Submission score:* 0.5!

========================================================
### Iteration 3

* I created a complex condition on features to choose accepted cases and for the rest I choose on random.
* Cross validation on training set only gives me a 0.47 result! it seems to be worse than the random solution. 
* I will not submit it!

========================================================
### Iteration 4 - Submitted 

* How about I try some sort naive Bayes algorithm?
* Just like looking for spam words, but here we use codes as words
* Lets encode all columns as strings with a suffix, to differentiate the same numbers between different features so for example resource 3000 -> 3000r, manager_id 500 -> 500m and then lets do naive Bayes on that 
* I wrote my own code for NB. It was a good excercise to learn how to do that.

*Submission score:* 0.60562

========================================================
### Iteration 5 - Submitted!
* ??

========================================================
### Iteration 6 - Submitted!

* Continue with Naive Bayes from iteration 4
* Updated the case what happens when we have a new word in the test set not yet in our dictionary
* Also, i think adding a full size of vocabulary in the denominator in Laplace smoothing is not a good idea. I noticed that it produces worse predictions on training set. In the most general case one adds alpha in numerator and beta in denominator. This gives us some freedom to tuning. I noticed that with smaller beta i got much better classification!
* Submission for alpha = 1 beta = 1

* Improvement! now better place in leader board 921! :)
* Result on a training set: 0.81

* Submission score:* 0.729


========================================================
### Iteration 7 
* Lets try one last time with naive Bayes and see if I can do higher score with different parameters alpha and beta alpha = 1 and beta = 100
* Not submitted, could not improve my score.

* Submission score:* 

========================================================
### Iteration 8 
* Here I want to try to use rattle package for random forest
* Did not finish this 

* Submission score:* 

========================================================
### Iteration 9 - Submitted! <- there is a mistake here, see submission 10!

* Here I want to try a simple solution given on Kaggle website
* http://www.kaggle.com/c/amazon-employee-access-challenge/forums/t/4938/825-using-only-excel-concat-pivot-and-vlookup-function
* In some sense this is similar to the Naive Bayes solution but apparently it seems to work better! - We will see
* So what I am doing is:
* Concatenate columns 3-10 in training set into long string for each row.
* Make a pivot table by the newly created factor (long string) where you average the action column
* Make a prediction for the training where a factor is matched to an average action. 
* Result for training set: 0.9999023 so i guess it overfits
* Make a prediction for the test set. Where a factor does not exist in test set, assign a value of mean for all averages in test set.

* Hmmm, something must have gone wrong. It us not very good result! Comparable with random!
* Lets try once more in Iteration 10

* Submission score:* 0.56538 

========================================================
### Iteration 10 - Submitted

* Here I continue my iteration 9 which gave a very low result!
* I think maybe there is something wrong in the way levels are set in the file and are mixed up in my analysis!
* I looked here at a code: https://gist.github.com/dylanjf/5879356 and learned functions aggregate and match :)
* I found a mistake! I was concatenating wrong columns! I took 2nd instead of 3rd.
* I improved myself up to 0.82459! :) - 811 place on leader board !
* It is nice because this solution is so fast!
* This result is exactly what people on the forum said it would be!
* The difference I noticed is that on forum they had ~4k missing values in test set after match, and I have about 7k! not sure why.

* Submission score:* 0.82459

========================================================
### Iteration 11 - Submitted

* On forum people say that a better result can be reached if I replace the NAs in the test set with prediction of a different model. For this case I could for example take my prediction from Naive Bayes model which took into account also partial fit of words instead of the whole sentence!
* So I repeat the whole Iteration 10 up to a moment where I replace the NAs.
* The score is worse wrrrrr
* I will wait with this step till I have some better predictions

* Submission score:* 0.79689

========================================================
### Iteration 12 - Submitted - Best model so far

* I want to play here with sparse matrices and try to apply logistic regression using glmnet package.
* This page is nice http://www.johnmyleswhite.com/notebook/2011/10/31/using-sparse-matrices-in-r/
* Hurray I improved!!!!
* Uff that was quite difficult
* 720 in leader board
* Interestingly the result is very close to what I got from cross-validation (cv.glmnet)
* Interestingly the AUC prediction is better if I use probabilities as predictions instead of classes 0/1 - for this I used the type="response" instead of type="class"


* *Submission score:* 0.86179

========================================================
### Iteration 13 - Submitted

* New idea based slightly on some forum discussions
* http://www.kaggle.com/c/amazon-employee-access-challenge/forums/t/5060/using-response-for-new-features
* I want to replace my current features with new ones 
* For example, for a manager MGRID = X I count how many times X was related to action=0 and how many times to action=1 and create two columns out of it MGRID0 and MGRID1 with these counts normalized by a total number of occurences of X in training set
* And then I treat these new values as numeric in my modeling. 
* Not sure yet what to do with new features in test set, but maybe best to put them on NA. Can't put them on 0 because it would put them on the same level as features with actual 0 count.
* I am fitting log10 of my features - there was a skew distribution of values. After log10 a histogram looks much better.
* I used classification tree to make a prediction
* Pruned tree with 6 levels

* Not an improvement, but not bad for such a simple model!

* *Submission score:* 0.78829

========================================================
### Iteration 14 - Submitted 

* Same as iteration 13 but I will try with random forest!
* O lala, very bad...

* *Submission score:* 0.61439

========================================================
### Iteration 15 - Submitted

* glm model, very simple, not bad at all
* I am fitting log10 of my features
* Cross validation is not correct here - see 16

* *Submission score:* 0.83817

========================================================
### Iteration 16 

* I am still looking into glm model - I managed to fix my cross-validation!
* In my code I create new features which take into account the value of the ACTION label
* At first for CV I was creating my new features in the entire training set and then doing CV on it
* But it is not correct, because the tests sets in CV will already contain information about the labels!
* What I need to do is to take my raw training set, in CV in each fold separately divide it into training and test set and then create my new features in training set and then based on this only create my test features
* In this way we do not include information from test set labels into the validation!
* Now my validation works much better and really predicts what I actually got from my submission!
* I like it!
* I don't need to submit it again, because I submitted it already in 15 and here I only improved CV

* *Submission score:* 

========================================================
### Iteration 17 - Submitted
* Ensemble of models
* I take some of my submissions 10,13,14,15 and take the average of their votes
* All the predictions where set to 0 or 1 (p>=0.5 to 1 and p<0.5 to 0)

* *Submission score:*  0.73989


========================================================
### Iteration 18 
* Ensemble of models
* I take some of my submissions 10,12,13,15 and take the average of their votes
* I leave here original values of predictions - probabilities
* I do not weight the predictions but submission score
* I improved!!!! Now 567 on leader board!

* *Submission score:*  0.88146

========================================================
### Iteration 19 - Submitted
* Ensemble as above but includes a result from 18 and also i take a weighted mean
* It gives me another tiny improvement :)
* 563 on leader board :)

* *Submission score:* 0.88173


========================================================
### Iteration 20 - Submitted
* Now that I have some better predictions, lets come back to my model 10 and 11 and replace missing values in test set with predictions from 12th iteration!
* Not an improvement, but not bad too

* *Submission score:* 0.82763

========================================================
### Iteration 21
* Take the model 16 and add some more features
* For example add how many times given code appears in data



========================================================
### Iteration 22
* I want to try one last approach based on a study of graphs for this data
* I create new feature for each column except resource and action
* for example:
* new feature is a number which for a given resource says how many unique managers it corresponds to
* or how many unique departments etc
* The reason is that if a resource is strictly assigned to a given department for example, then if it was
requested from several different departments, there is a probability it will be rejected for one of them. 
* Then I will try some models on thsi data to see how it works

* *Submission score:* 




- - - 
========================================================
### ROC curve intuition 
* Here I am trying to understand how the ROC curve mechanics works for my submissions
* I like this article about ROC curves:
* ftp://norbif.uio.no/pub/outgoing/runeho/KR/Metz78SeminNuclMed8-283.pdf

















