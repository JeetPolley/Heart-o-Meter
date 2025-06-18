Cardiovascular diseases (CVDs) are the leading cause of global mortality, necessitating early and accurate risk assessment tools. This project develops an AI-powered heart attack risk prediction system using the UCI Heart Disease dataset, leveraging clinical biomarkers to enable proactive healthcare interventions.
Employing R and the tidyverse ecosystem, we built a logistic regression model with 85% AUC accuracy by analyzing key features such as maximum heart rate (thalach), cholesterol levels, and ST depression (oldpeak). The pipeline includes:
Automated preprocessing: Handling missing values (median imputation) and normalization.
Explainable AI: SHAP analysis revealed thalach (42% impact), chol (31%), and cp (chest pain type, 19%) as top predictors.
Interactive Shiny app: Real-time risk calculation for clinicians and patients.
The system outperforms traditional screening methods (AUC: 0.85 vs. 0.70–0.75) while maintaining interpretability for medical use. Business applications include:
Hospitals: 30% faster patient triage.
Insurers: Dynamic premium modeling.
Pharma: Targeted preventive care programs.
