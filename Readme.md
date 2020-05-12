Files

The files are listed in the order to run.

download_preprocess.R - code to download and preprocess the movielens data: There are two methods to run the file. choose_method = 0 automatically downloads the preprocessed data from Dropbox whereas choose_method = 1 downloads data from grouplens website and pre-processes locally.

tuneparams.R - code to tune models involving models with matrix factorization using stochastic gradient descent.

analysis.R - code to analyse data and results

Reports

Movielens_Report_pdf - pdf report of the project

Movielens_Report_html - HTML report of the project

HTML rendered output is at 

Description

In this project, we study the 10M Movielens dataset provided by the GroupLens research lab to predict movie ratings of an user. We use some of the techniques developed during the Netflix Challenge to achieve an RMSE of about .78.
