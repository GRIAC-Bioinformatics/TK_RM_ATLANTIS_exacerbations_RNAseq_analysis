
library(dplyr)
library(tableone)

setwd("~/Work/RP2/ATLANTIS")

atlantis_exacerbations <- read.csv(file = './Exacerbations/ATLANTIS_master_table_exacerbation.csv')
atlantis_exacerbations <- atlantis_exacerbations %>%
  dplyr::select(-X) %>%
  filter (QC_check == 'YES') %>%
  mutate(gender = as.factor(gender),
         smoking.status = as.factor(smoking.status),
         exacerbation = as.factor (if_else(exacerbation== TRUE, 'YES','NO')),
         time_to_exacerbation = as.numeric(time_to_exacerbation),
         follow_up = as.numeric(follow_up),
         Ex_freq = as.factor(if_else(NUM_EX_D>1, 'freq', 'not_freq'))) %>%
  mutate(NUM_EX_per_YEAR = if_else(exacerbation == TRUE, 365/follow_up*NUM_EX_D, as.double(NUM_EX_D)))

atlantis_exacerbations <- atlantis_exacerbations %>%
  mutate(follow_up_250more = if_else(follow_up>=250, 'YES', 'NO'))

#### filter out censored samples: 
atlantis_exacerbations <- atlantis_exacerbations %>%
  filter(follow_up_250more == 'YES')

### add extra data 
big_master_table <-  read.csv('./atlantis_patient_data.csv', header =TRUE, na.strings=c("","NA"))

clinical_table <- big_master_table[,c('PT', 'VISIT_n', 'PACKNO','BMI','PHADRES', 'DUR_DIS','AGE_DIAG','B_FEV1F','GINA', 
                                      'NUM_EX',"MORE1EX", 'LAMA','BIO','SYS_COR',
                                      'ICS', 'ICS_LABA', 'ICS_MEAN_DOSE','ICS_DDOSE_EQ','ICS_LABA_DDOSE_EQ',
                                      'acq6_score','LABEOSV','LABNEUV','LABMACV','BRONCHP', 'LYMPHOP', 'EOSP',
                                      'MACROP', 'NEUTROP',
                                      'B_TLCPNVF', 'B_RVTLCPNVF', 'B_FEV1PNVG','B_FEV1FPNVG', 'FENRES', 'PCD',
                                      'B_R520PNVR', 'B_SCONDPNVF','B_SACINPNVF', 'B_F50PNVR', 'LA', 'WA','TA',
                                      'Pi10', 'WA_TA100', 'VI_856', 'VI_950', 'lung_ratio', 'MLD_ratio',
                                      'B_RVTLCPNVF',  'B_R520PNVR', 'B_SCONDPNVF','B_SACINPNVF', 'B_F50PNVR')]

clinical_table <- clinical_table %>% #clinical_table[!duplicated(clinical_table$PT), ]%>%
  filter(VISIT_n == 1) %>%
  mutate (GINA = as.factor(GINA),
          PHADRES =as.factor(PHADRES),
          LAMA = as.factor(LAMA),
          BIO = as.factor(BIO),
          SYS_COR = as.factor(SYS_COR))

atlantis_exacerbations <- atlantis_exacerbations%>%
  left_join(clinical_table, by = c('PT'='PT'))%>%
  mutate(Ex_last_y = if_else(NUM_EX > 0, 'YES', 'NO')) %>%
  mutate(any_ICS = if_else(ICS == "No" & ICS_LABA == "No", "No", "Yes")) %>%
  mutate(ICS_dose_sum = if_else(is.na(ICS_DDOSE_EQ) & is.na(ICS_LABA_DDOSE_EQ), NA,
                                coalesce(ICS_DDOSE_EQ,0) + coalesce(ICS_LABA_DDOSE_EQ,0))) %>%
  mutate(GINA_mild_severe = if_else(GINA %in% c('1','2','3'), 'mild', 'severe'))%>%
  mutate(smoking = if_else (smoking.status %in% c('Non.smoker'), 'never', 'ex_cur_smoker'))


non_normally <- c('DUR_DIS', 'AGE_DIAG','PACKNO','acq6_score', 'LABEOSV', 'BRONCHP', 'LYMPHOP',
                  'MACROP', 'NEUTROP', 'B_R520','VI_856', 'VI_950','FENRES', 'EOSP','B_R520PNVR', 
                  'B_SCONDPNVF','B_SACINPNVF', "NUM_EX", 'ICS_dose_sum')

var_for_table <- c('gender','age','smoking.status','PACKNO','BMI','PHADRES', 'DUR_DIS','AGE_DIAG','B_FEV1F','GINA', 'NUM_EX','LAMA','BIO',
                   'SYS_COR', 'any_ICS', 'ICS_dose_sum',
                   'acq6_score','LABEOSV','LABNEUV','LABMACV','BRONCHP', 'LYMPHOP', 'EOSP', 'MACROP', 'NEUTROP',
                   'B_TLCPNVF', 'B_FEV1PNVG','B_FEV1FPNVG', 'FENRES', 'PCD','bhr',
                   'B_RVTLCPNVF',  'B_R520PNVR', 'B_SCONDPNVF','B_SACINPNVF', 'B_F50PNVR',
                   'VI_856', 'VI_950', 'lung_ratio','MLD_ratio', "MORE1EX", "DIA","SYS", "GINA_mild_severe", "smoking")

tabtotal <- CreateTableOne(vars = var_for_table, strata = "exacerbation" , data = atlantis_exacerbations)

Final_statistics <- print(tabtotal, nonnormal = non_normally,
                          exact = "stage", formatOptions = list(big.mark = ","), quote = FALSE, noSpaces = TRUE)
Final_statistics <- Final_statistics%>%
  as.data.frame()

### Make nice table:
rownames(Final_statistics)[rownames(Final_statistics) == "age (mean (SD))"] = "Age (mean (SD))"
rownames(Final_statistics)[rownames(Final_statistics) == "DUR_DIS (median [IQR])"] = "Disease duration (median [IQR])"
rownames(Final_statistics)[rownames(Final_statistics) == "AGE_DIAG (median [IQR])"] = "Age of diagnosis (median [IQR])"
rownames(Final_statistics)[rownames(Final_statistics) == "acq6_score (median [IQR])"] = "ACQ-6 total mean score (median [IQR])"
rownames(Final_statistics)[rownames(Final_statistics) == "LABEOSV (median [IQR])"] = "Blood eosinophils (median [IQR])"
rownames(Final_statistics)[rownames(Final_statistics) == "Ex_last_y = YES (%)"] = "Exacerbations last year (n(%))"
rownames(Final_statistics)[rownames(Final_statistics) == "FENRES (median [IQR])"] = "FeNO (median [IQR])"
rownames(Final_statistics)[rownames(Final_statistics) == "GINA (%)"] = "GINA (n(%))"
rownames(Final_statistics)[rownames(Final_statistics) == "B_FEV1PNVG (mean (SD))"] = "FEV1 % of predicted normal value (mean (SD))"
Final_statistics$NO = sub(', ', '-', Final_statistics$NO)
Final_statistics$YES = sub(', ', '-', Final_statistics$YES)
  
write.table(Final_statistics, file = "./Exacerbations/Baseline_statistics_exacerbations.csv",
          sep = '\t', quote =FALSE, col.names = TRUE, row.names = TRUE, dec = '.')

