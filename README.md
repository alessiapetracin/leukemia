### leukemia ###

---

### Introduction ###

The present analysis focuses on gene expression for 2,000 genes on patients with leukemia, who belong to two subgroups: patients with a chromosomal translocation ("1") and patients cytogenetically normal ("-1").
We will perform a supervised analysis for prediction of the two subgroups using support vector machines.
The data are provided in the `gene_expr.tsv` file, containing expression for 2,000 genes and an additional column with patient labels.

Three models are evaluated: 
- support vector classifier
- polynomial kernel
- radial kernel

As a second approach, we filter the dataset. 
A popular approach in gene expression analysis is to keep only the most variable genes for downstream analysis. Since most of the 2K genes have low expression or do not vary much across the experiments, this step usually minimizes the contribution of noise. 
We select then only genes whose standard deviation is among the top 5% and repeat the analyses performed in the previous task on the filtered data.
We then evaluate the models through ROC curves.

---

### How to run the project ###

Download the zip file containing the dataset and the code files.
Unzip it and open the files in an .R environment, supporting the libraries:
- e1071
- caret
- ROCR

