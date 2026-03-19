# Data Codebook  
  
## Overview  
This document describes the structure of the dataset used in the study:  
  
"Sustainable AI research? Researchers’ perceptions of the environmental impact of AI and emissions-feedback tools – a mixed methods study"  
  
Due to privacy and ethical considerations, the original dataset cannot be shared.    
A template dataset (`data_template.xlsx`) is provided to illustrate the structure.  
  
---  
  
## General Structure  
  
- Each row represents one survey respondent  
- Each column represents a survey variable  
- Likert-scale items use standardized response categories  
  
---  
  
## Variables  
  
### ID  
- Unique respondent identifier  
- Type: Integer  
  
---  
  
### Survey Items (Likert Scale)  
  
The following variables capture responses to statements about sustainable AI practices:  
  
- Awareness    
- Incentive    
- TeachEfficientCode    
- LostInMetrics    
- ConflictGoals    
- RestrictedFreedom    
  
#### Response scale:  
- "Strongly disagree"  
- "Disagree"  
- "Agree"  
- "Strongly agree"  
- "no opinion" 
  
---  
  
### Item Wording  
  
- **Awareness**    
  "... would increase my awareness of the environmental impact of my work"  
  
- **Incentive**    
  "... would be an incentive to use less computing resources"  
  
- **TeachEfficientCode**    
  "... would – over time – teach me how to create more energy efficient models"  
  
- **LostInMetrics**    
  "... would get lost among all the other metrics I receive after training a model"  
  
- **ConflictGoals**    
  "... would make me feel a conflict between energy efficiency and my personal or academic goals and tasks"  
  
- **RestrictedFreedom**    
  "... would make me feel restricted in my academic freedom"  
  
---  
  
### Additional Survey Variables  
  
#### SustainableAI_Definition  
- Open-text response  
- Participant's definition of "Sustainable AI"  
  
#### AI_EnvImpact_PublicDebate  
- Likely Likert or categorical (specify if needed)  
  
#### AI_NegEnvImpact_10yrs  
- Likely Likert or categorical (specify if needed)  
  
#### Optional_Comments  
- Open-text responses  
- Additional participant comments  
  
---  
  
## Demographics  
  
### Age  
- Self-reported age group or value  
- Type: Categorical or numeric (depending on dataset)  
  
---  
  
### Gender  
- Self-reported gender  
- Categories may include:  
  - Female  
  - Male  
  - Non-binary  
  - Prefer not to say  
  - Other (free text possible)  
  
---  
  
### AI_Researcher_Job  
- Indicates whether respondent works in AI research  
- Type: Categorical / binary  
  
---  
  
### Title_Education  
- Highest educational attainment  
  
#### Recoded categories used in analysis:  
- "Bachelor / Master"  
- "PhD"  
- "Habilitation" (if applicable)  
  
---  
  
## Data Processing Notes  
  
- Likert responses are treated as ordered categorical variables  
- "no opinion" responses are:  
  - Visualized separately (Figure 3), or  
  - Excluded from some subgroup analyses  
- Percentages are calculated per item (within-category normalization)  
  
---  
  
## Reproducibility Notes  
  
- All analysis scripts assume the data is stored in:  
  `data/Analysis_for_R.csv`  
  
- Column names must match exactly as defined above  
  
---  
  
## Contact  
  
For questions about the dataset or analysis, please contact:  

Anonymous
