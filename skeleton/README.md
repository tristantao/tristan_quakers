This folder contains code to create error diagrams, calcuate area under error diagram, reproduce Leun's result, and test on two more models: MDA Div and MDA Sub
* Functions to create error diagrams and calcuate area under error diagram are in [SupportFunctions](./SupportFunctions) folder.
* **A demonstration on how to use SupportFunctions are in [Demo](./Demo)**.
* Functions to reproduce Leun's result and analyze the MDA Model `(W(M) = k * (mu ^M))` using our own code is in [MDA](./MDA) folder.
* Functions to produce and analyze MDA Div Model `W(M) = k * (mu ^M / M)` is in [MDAScaledByDiv](./MDAScaledByDiv) folder.  
* Functions to produce and analyze MDA Sub Model `W(M) = k * (mu ^M - tM)` is in [MDAScaledBySub](./MDAScaledBySub) folder.


How to run the code:
--------------------
Please clone the repository
* **If you want to play around with the results:** all of the data are in the `.RData` file. And the `xxAnalysis.R` files serve as examples on how to analyze the data. 
* **If you want to draw Error Diagram for your own model**, you can use the functions in [SupportFunctions](./SupportFunctions). MDA, MDASub, and MDADiv all use these functions. Ussage is in the comment of the scripts. Please refer to `xxModelGetData.R` for examples on how to use them.
* **A demonstration on how to use `SupportFunctions` are in [Demo](./Demo)**.
* **If you want to reproduce the models**, you need to have [DataFrame.csv](https://www.dropbox.com/s/tzx4qqxhh9u9iz2/DataFrame.csv) in this folder. Please `setwd("[Where you save the repository]/TheQuakers/skeleton/")`before running the code.
* MDA and MDADiv takes about 1 hour(21 models each). 
* MDAsub takes about 8 hours(210 models).  

