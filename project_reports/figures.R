library(dplyr)
library(dbplyr)
devtools::load_all()

# 01_intro

# 286k monthly average
careHomePrescribingScrollytellR::patients_by_prescribing_status_df %>%
  ungroup() %>%
  filter(PRESCRIBING_STATUS == "Received care home prescribing") %>%
  summarise(mean(SDC_TOTAL_PATIENTS))
