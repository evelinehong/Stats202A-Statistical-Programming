# -*- coding: utf-8 -*-
"""

 Stat 202A 2019 Fall - Homework 07
 Author: 
 Date : 

 INSTRUCTIONS: Please fill in the corresponding function. Do not change function names, 
 function inputs or outputs. Do not write anything outside the function.
 
"""

import numpy as np
from sklearn.datasets import load_breast_cancer
from sklearn.model_selection import KFold, GridSearchCV
from matplotlib import pyplot as plt
import xgboost as xgb

cancer = load_breast_cancer()
X = cancer.data
y = cancer.target

#############################################################################################################
# TODO (1) Perform 5-fold validation for cancer data.
# You may consider use KFold function in sklearn.model_selection
# Print the mean and std of the 5-fold validation accuracy
#############################################################################################################
kfold = KFold(5)
print 
acc = []
i = 0
for m,n in kfold.split(X):
    i += 1
    X_train, X_test = X[m], X[n]
    y_train, y_test = y[m], y[n]
    xgb_model = xgb.XGBClassifier(objective="binary:logistic", random_state=42)
    xgb_model.fit(X_train, y_train)

    y_pred = xgb_model.predict(X_test)
    Xpos = X_test[y_pred == 1.0]
    Xneg = X_test[y_pred == 0.0]
    plt.figure()
    plt.scatter(Xpos[:, 0], Xpos[:, 1], c = 'r', label = 'Postive')
    plt.scatter(Xneg[:, 0], Xneg[:, 1], c = 'b', label = 'Negative')
    plt.legend()
    plt.title("Graph of 5-fold: split" + str(i))
    plt.savefig("Graph of 5-fold: split" + str(i) + ".png")

    acc.append(sum(y_pred == y_test)/len(y_test))
acc = np.array(acc)
print("Q1: 5-fold validation")
print("Mean: {}, Std: {}".format(acc.mean(), acc.std()))
#############################################################################################################
# TODO (2) Perform Grid Search for parameter max_depth in [3,5,7] and min_child_weight in [0.1, 1, 5]
# For each combination use 5-Fold validation
# You may consider use GridSearchCV function in sklearn.modelselection
# Print the grid search mean test score for each paramter combination (use cv_results_)
#############################################################################################################
d_candidate = [3,6,9,10]
w_candidate = [0,2,4,6]
parameters = {"max_depth":d_candidate, "min_child_weight":w_candidate}
xgb_model = xgb.XGBClassifier(objective="binary:logistic", random_state=42)
clf = GridSearchCV(xgb_model, parameters, cv=5)
clf.fit(X, y)
print('Q2:', clf.cv_results_['params'])
print('Q2:', clf.cv_results_['mean_test_score'])

params = clf.cv_results_['params']
for param in params:
    xgb_model = xgb.XGBClassifier(objective="binary:logistic", random_state=42, **param)
    xgb_model.fit(X, y)
    y_pred = xgb_model.predict(X)
    Xpos = X[y_pred == 1.0]
    Xneg = X[y_pred == 0.0]
    plt.figure()
    plt.scatter(Xpos[:, 0], Xpos[:, 1], c = 'r', label = 'Postive')
    plt.scatter(Xneg[:, 0], Xneg[:, 1], c = 'b', label = 'Negative')
    plt.legend()
    plt.title("Graph of parameter:"+str(param))
    plt.savefig("Graph of parameter:"+str(param)+".png")
 
 
#############################################################################################################
# TODO (3) Plot the feature importance of the best model
# You may fit a new xgboost model using all the data and then plot the importance using xgb.plot_importance()
#############################################################################################################
best_ind = np.argsort(clf.cv_results_['mean_test_score'])[-1]
best_params = clf.cv_results_['params'][best_ind]
print('Q3: best param', best_params)
print('Q3: best result', clf.cv_results_['mean_test_score'][best_ind])
best_xgb_model = xgb.XGBClassifier(objective="binary:logistic", random_state=42, **best_params)
best_xgb_model.fit(X, y)
plt.bar(range(len(best_xgb_model.feature_importances_)),
        best_xgb_model.feature_importances_)
plt.show()
