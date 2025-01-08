# Lin-et-al-2025-Nature-Aging-paper-

This resource includes the R code required to reproduce the results described in our paper.

The master do-file, “Master_Do_File_Run_ALL.do”, defines the project working directory, creates all subfolders, and calls all relevant do-files, enabling the replication of all analyses in the study. Users must update the working directory with their own path. Raw data from HRS, ELSA, SHARE, and CHARLS should be saved in the “data/rawdata” folder within their respective subfolders.

Replication of All Analyses
•	Run “code/Master_Do_File_Run_ALL.do”

Alternatively, users can run individual do-file but must define the project working directory if not using the master do-file. Additionally, the analytical data should be prepared first by running the code in Step 1. All steps and corresponding codes are outlined as follows. 

Step 1: Prepare Analytical Data
•	Run “code/Main/Global_Dementia_LTC_PData.do”

Step 2: Main Analysis - Absence of Care Trend (Fig.1)
•	Run “code/Main/Global_Dementia_LTC_Trend.do”

Step 3: Main Analysis - Disparities (Fig.2 & Fig.3)
•	Run “code/Main/Global_Dementia_LTC_Disparities_Edu.do” (Fig.2)
•	Run “code/Main/Global_Dementia_LTC_Disparities_LiveAlone.do” (Fig.3)

Step 4: Main Analysis - Descriptive Analysis (for statistics shown in Table 1 & Extended Data Fig.1) 
•	Run “code/Descriptive/Global_Dementia_LTC_ Descriptive.do” 

Step 5: Additional Analysis - Disability Trend (Extended Data Fig.2) 
•	Run “code/Descriptive/Global_Dementia_LTC_ Disability.do” 

Step 6: Sensitivity Analyses (Extended Data Figs. 3 and 4)
•	Run “code/Robustness/Global_Dementia_LTC_Robust_Cog25.do” (Extended Data Fig.3)
•	Run “code/Robustness/Global_Dementia_LTC_Robust_Demented.do” (Extended Data Fig.4)

Step 7: Additional Analysis - Absence of Care for PLWD with More Severe Limitations (Extended Data Fig.5)
•	Run “code/Robustness/Global_Dementia_LTC_Robust_MoreDisabled.do”

Step 8: Supplementary Analysis among Non-Dementia Sample (Extended Data Figs. 6 and 7)
•	Run “code/Supplementary/Global_Dementia_LTC_Descriptive_NonDemented.do” (Supplementary Table S7)
•	Run “code/Supplementary/Global_Dementia_LTC_Disability_NonDemented.do” (Extended Data Fig.6)
•	Run “code/Supplementary/Global_Dementia_LTC_Trend_NonDemented.do” (Extended Data Fig.7)
![image](https://github.com/user-attachments/assets/be71a5b0-2ac5-42bd-ac09-2689c2a305ed)
