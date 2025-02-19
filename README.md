We had realized to study (EDA-exploration data analysis) about "Coimbra Breast Cancer" dataset.

The original data includes clinical observations from 64 patients with breast cancer and 52 healthy controls, encompassing 10 quantitative predictors and a binary dependent variable indicating the presence or absence of breast cancer.

The data from the Coimbra hospital show 8 biomarkers-predictors of blood analysis (bmi, glucose, insulin, HOMA, leptin, adiponectin, resistin, MCP.1) plus a binary variable (classification - 1. Healthy control patient 2- Cancer patient) apart from age.

Through the graphs, detect the following correlation of predictors.

glucose(metabolic indicator)→insulin(hormone)→HOMA(beta cell function)→resistin(protein)

When a patient has cancer, their blood glucose levels are higher than those of healthy control patients. This is because cancer cells that reproduce in an uncontrolled manner need more glucose than normal for their metabolic activity.

This biomarker produces a chain reaction at a chemical level; as cancer cells increase, glucose increases insulin levels in the blood.

In turn, increasing insulin increases the function of beta cells (HOMA).

Also, increasing insulin increases resistance to it through resistin.

To optimize the accuracy of the analysis methods (machine learning) would be necessary:

On the one hand, more patient records in this case we only have 116 records.

On the other hand, the more biomarkers the blood test has, more biomarker correlations we can detect.

Two more fields are also needed to know if the patient has diabetes or not and another that tells us the type I or II diabetes. This allows us to rule out false positives.

That is why in the PET test (positron emission tomography) TAC (computerized axial tomography) they use the radio tracer F18 -Fluorodeoxyglucose that acts with positrons marking the areas of greater metabolic activity.

This test determines the type of cancer by its location to know which treatment or surgery to apply.

The final conclusion I draw from the 4 biomarkers (glucose, insulin, HOMA, resistin) that could serve as indicators to detect cancer, since they are all derived (correlations) from glucose, would be the one that works for us.

The advantage that this marker would not only be valid for breast cancer, it is valid for any type of cancer. This is because, regardless of the type of cancer cell, they all need more glucose than healthy cells.

Today, cancer screening tests can be done with blood tests using tumor markers, and glucose could be taken into account as another indicator. It must be taken into account that if the patient has diabetes, this marker would not be useful for detecting it.

If we use more biomarkers in blood tests, we will have more predictors to detect future cancer indicators.

The results obtain to accuracy in different machine learning methods are:

Logistic regression: 0.79167

Loess: 0.66667

K nearest neighbours: 0.83333

Random forest:0.83333

Ensemble (median combined LM, LOESS, KNN, RF): 0.79167
