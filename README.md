# Overview 

We implement four different fuzzy inference systems, where two of them are based on the adaptive neuro-fuzzy inference systems (ANFIS) for predicting the mass yield of extractions performed to obtain chia extract from chia cake. Chia cake is a sub-product of conventional extractions of chia extract from chia seeds, which contains several health-promoting properties like omega-3 fatty, antioxidants, and vitamins. The idea of performing extractions from chia cake is that the cake also contains these health-promoting properties. 

The fuzzy systems are modeled to build an expert system that predicts the mass yield of the chia cake extract, which is an important measure to analyze the efficiency of an extraction, by using three important parameters employed by extractions: (i) solvent, (ii) temperature, and (iii) extraction time (i.e., period). We make use of a real dataset (available together with this implementation) that contains several observations of real extractions. 

As a result, our implementation provides tables containing accuracy measures i.e., MAE, RMSE, MAPE, R²) that allow us to identify the most promising fuzzy system for the problem. Further, our implementation is fully reproducible and its methodology of development can be extensible to deal with similar problems, such as the prediction of the mass yield or other measures in other organic matrices

# How to use?

This project is also publicly available in CodeOcean [here](https://codeocean.com/capsule/5002128/tree/v1). Hence, you can execute our code and obtain the results. It is interesting to note that our main script invokes the function `execute_repeated_holdout`, which executes the holdout validation repeated _k_ times where _k_ is the second parameter of this function (the default value is 5).

# Associated research paper

We have employed the implementation of these fuzzy systems in the following research paper:

- [Johann, G.; Santos, C. S.; Montanher, P. F; Oliveira, R. A. P.; Carniel, A. C. Applying Fuzzy Inference Systems in the Extraction of Chia Cake Extract: Predicting the Mass Yield. In: 2021 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE), 2021, pp. 1-6.](https://ieeexplore.ieee.org/document/9494541)
