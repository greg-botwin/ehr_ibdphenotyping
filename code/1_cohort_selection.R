# connect to database
options(java.parameters = "-Xmx8048m")
library(RJDBC)
library(DBI)
library(dbplyr)
library(tidyverse)
library(lubridate)
library(tableone)

# Create a Driver Object in R
jdbcDriver =JDBC("oracle.jdbc.OracleDriver",classPath="../programs//OJDBC-Full/ojdbc6.jar")

con  <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//RDSPENTORA01.csmc.edu:1533/RDSPENT1",
                  "ibd_database", "ibd_database1")

sql_translation.JDBCConnection <- dbplyr:::sql_translation.Oracle
sql_select.JDBCConnection <- dbplyr:::sql_query_select.Oracle
sql_subquery.JDBCConnection <- dbplyr:::sql_query_wrap.Oracle


# function to create table connections
create_table <- function(table_name, schema, con){
  tbl(con, in_schema(schema = schema, table = table_name))
}


# get tables
ibd_table_names <- c("LVS_SUBJECT",
                     "LVS_PARTICIPANT",
                     "IBD_SUBJECTS_DEMO",
                     "IBD_DIAGNOSIS", 
                     "MICS_VALUES_DEFAULT",
                     "MICS_IDENTITY",
                     "IBD_ENCOUNTERS")

# apply function across desired tables
tables_db <- sapply(ibd_table_names, create_table, con = con, schema = "IBD_DATABASE",
                    USE.NAMES = TRUE, simplify = FALSE)
list2env(tables_db, globalenv())

# goal is to create two cohorts each annotated with approriate metadata
# ehr cohort: 
# MIRIAD Active (active flag = y, Study 69 IBD Cohort, Enrollment Status = Enrolled)
# IBD diagnosis of cd or uc
# at least 1 dx code that maps to ccs
# genetic cohort:
# ehr cohort with imputed ichip data
# meta data: genderflag, birthdt, current diagnosis, genetic id, mrn, mics id 
# computed meta data: length of follow up, number of unique encounters

# create files and data frames
## ccs file
ccs <- read_csv("DXCCSR_v2022-1.CSV")
colnames(ccs) <- make.names(colnames(ccs))
ccs_chapter <- read_csv("ICD-10-CM Diagnosis Chapter.csv")
colnames(ccs_chapter) <- make.names(colnames(ccs_chapter))
ccs <- ccs %>%
  mutate(across(.cols = everything(), ~str_remove_all(.x, "'"))) %>%
  mutate(dx_chapter = str_sub(X.Default.CCSR.CATEGORY.OP., start = 1, end = 3)) %>%
  left_join(., ccs_chapter, by = c("dx_chapter" = "X3.Character.Abbreviation")) %>%
  select(X.ICD.10.CM.CODE., X.ICD.10.CM.CODE.DESCRIPTION., X.CCSR.CATEGORY.1.,
         X.CCSR.CATEGORY.1.DESCRIPTION., ICD.10.CM.Diagnosis.Chapter)


## active subjects
if(file.exists("data/miriad_active_ibd_subjects.csv")){
  active_ibd_subjects <- read_csv("data/miriad_active_ibd_subjects.csv")
} else {
  active_subjects <- LVS_SUBJECT %>%
    filter(ACTIVEFLAG == "Y") %>%
    select(MRN, S_SUBJECTID, GENDERFLAG, BIRTHDT) %>%
    inner_join(., LVS_PARTICIPANT, by = c("S_SUBJECTID" = "SUBJECTID")) %>%
    filter(ACTIVEFLAG == "Y") %>%
    filter(SSTUDYID == "Study-20-000069") %>%
    filter(CPCOHORTID == "IBD, non-IBD, family") %>%
    filter(PARTICIPANTSTATUS == "Enrolled") %>%
    select(S_SUBJECTID, S_PARTICIPANTID, MRN, EXTERNALPARTICIPANTID, GENDERFLAG, BIRTHDT) %>%
    inner_join(., MICS_IDENTITY, by = c("S_SUBJECTID"= "S_SUBJECTID")) %>% 
    select(S_SUBJECTID, S_PARTICIPANTID.x, MRN.x, EXTERNALPARTICIPANTID.x, GENDERFLAG.x, BIRTHDT.x, RECORD_ID) %>%
    rename(S_PARTICIPANTID = S_PARTICIPANTID.x,
           MRN = MRN.x,
           EXTERNALPARTICIPANTID = EXTERNALPARTICIPANTID.x, 
           GENDERFLAG = GENDERFLAG.x, 
           BIRTHDT = BIRTHDT.x)
  
  ## get current diagnosis 
  ibd_subjects <- MICS_VALUES_DEFAULT %>%
    filter(TRCS_PROPERTYID == "8580") %>%
    select(RECORD_ID, VALUE) %>%
    filter(VALUE %in% c("Crohn's Disease", "Ulcerative Colitis")) %>%
    distinct() %>%
    rename(current_diagnosis = VALUE)
  
  ## filter to only inlucd ibd subjects
  active_ibd_subjects <- active_subjects %>%
    inner_join(., ibd_subjects, by = "RECORD_ID") %>%
    collect() %>%
    distinct(MRN, .keep_all = TRUE)
  
  write_csv(active_ibd_subjects, "data/miriad_active_ibd_subjects.csv")
}

## ibd encounters
if (file.exists("data/miriad_encounters_for_active_subjects.csv")){
  active_encounters <- read_csv("data/miriad_encounters_for_active_subjects.csv") 
} else {
  active_encounters <- IBD_ENCOUNTERS %>%
    collect() %>%
    filter(!is.na(ENC_TYPE)) %>%
    filter(MRN %in% active_ibd_subjects$MRN)
  
  write.csv(active_encounters, "data/miriad_encounters_for_active_subjects.csv")
}

## dx codes
### include all icd codes with dx date >= 01-01-2005 and <= 12-31-2021
if (file.exists("data/miriad_diagnosis_codes_active_subjects.csv")){
  active_icd <- read_csv("data/miriad_diagnosis_codes_active_subjects.csv") 
} else {
  active_icd <- IBD_DIAGNOSIS %>%
    collect() %>%
    mutate(DIAGNOSIS_DATE = dmy(DIAGNOSIS_DATE)) %>%
    filter(between(DIAGNOSIS_DATE, mdy("01-01-2005"), mdy("12-31-2021"))) %>%
    filter(MRN %in% active_encounters$MRN) %>%
    filter(!is.na(ICD10_CODE)) %>%
    separate_rows(ICD10_CODE, sep = ", ") %>% # seperate collapsed icd codes
    mutate(ICD10_CODE = str_remove(ICD10_CODE, "\\.")) %>%
    filter(ICD10_CODE %in% ccs$X.ICD.10.CM.CODE.)
  
  write.csv(active_icd, "data/miriad_diagnosis_codes_active_subjects.csv")
}

## dx codes mapped
if (file.exists("data/miriad_diagnosis_codes_active_subjects_ccs_mapped.csv")){
  active_icd_ccs_mapped <- read_csv("data/miriad_diagnosis_codes_active_subjects_ccs_mapped.csv") 
} else {
  active_icd_ccs_mapped <- active_icd %>%
    inner_join(., ccs, by = c("ICD10_CODE" = "X.ICD.10.CM.CODE."))
  
  write.csv(active_icd_ccs_mapped, "data/miriad_diagnosis_codes_active_subjects_ccs_mapped.csv")
}

# derived metrics 
## count of unique encounters per individual
unique_encounters <- active_encounters %>%
  distinct(MRN, ENC_TYPE) %>%
  count(MRN, name = "n_unique_encounters")

active_ibd_subjects_meta <- active_ibd_subjects %>%
  left_join(., unique_encounters, by = "MRN") %>%
  mutate(n_unique_encounters = if_else(is.na(n_unique_encounters), as.integer(0), n_unique_encounters))

## followup duration
followup_duration <- active_icd %>%
  group_by(MRN) %>%
  summarise(max_date = max(DIAGNOSIS_DATE, na.rm = TRUE),
            min_date = min(DIAGNOSIS_DATE, na.rm = TRUE)) %>%
  mutate(follow_duration = as.numeric(max_date - min_date, unut = "days"))

active_ibd_subjects_meta <- active_ibd_subjects_meta %>%
  inner_join(., followup_duration, by = "MRN")

## age as of mdy("12-31-2021")
active_ibd_subjects_meta <- active_ibd_subjects_meta %>%
  mutate(age = trunc(BIRTHDT %--% mdy("12-31-2021") / years(1)))

## number of icd codes per participant
n_icd <- active_icd %>%
  count(MRN, name = "n_icd")

active_ibd_subjects_meta <- active_ibd_subjects_meta %>%
  inner_join(., n_icd, by = "MRN")

### n Unique ICD Codes per participant
n_unique_icd <- active_icd %>%
  distinct(MRN, ICD10_CODE) %>%
  count(MRN, name = "n_unique_icd")

active_ibd_subjects_meta <- active_ibd_subjects_meta %>%
  left_join(., n_unique_icd, by = "MRN")

### Distribution of Chapters per particiapnt
n_unique_ccs_chapters <- active_icd_ccs_mapped %>%
  distinct(MRN, ICD.10.CM.Diagnosis.Chapter) %>%
  count(MRN, name = "n_unique_ccs_chapters")

active_ibd_subjects_meta <- active_ibd_subjects_meta %>%
  left_join(., n_unique_ccs_chapters, by = "MRN")

### Distribution of Number of Categories
n_unique_ccs_categories <- active_icd_ccs_mapped %>%
  distinct(MRN, X.CCSR.CATEGORY.1.DESCRIPTION.) %>%
  count(MRN, name = "n_unique_ccs_categories")

active_ibd_subjects_meta <- active_ibd_subjects_meta %>%
  left_join(., n_unique_ccs_categories, by = "MRN")

write_csv(active_ibd_subjects_meta, "data/active_ibd_subjects_meta.csv")

# counts for schema
## active subjects
active_ibd_subjects %>%
  distinct(MRN) %>%
  count()

## active subejcts with an encounter id
active_encounters %>%
  distinct(MRN) %>%
  count()

## active subejcts with an encounter and an icd code
active_icd %>%
  distinct(MRN) %>%
  count()

# tables
myVars <- c("GENDERFLAG", "current_diagnosis", "n_unique_encounters", "age", 
            "follow_duration", "n_icd", "n_unique_icd",
            "n_unique_ccs_chapters", "n_unique_ccs_categories")
catVars <- c("GENDERFLAG", "current_diagnosis")
nonnormal <- c("n_unique_icd", "n_icd")
## overall table one
tab1 <- CreateTableOne(vars = myVars, data = active_ibd_subjects_meta,
                       factorVars = catVars)
summary(tab1)
tab1print <- print(tab1, nonnormal = nonnormal, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
write.csv(tab1print, file = "tables/features_overall_ehr_cohort.csv")

## table one by dx
tab2 <- CreateTableOne(vars = myVars, data = active_ibd_subjects_meta,
                       factorVars = catVars, strata = "current_diagnosis")

tab2print <- print(tab2, nonnormal = nonnormal, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
write.csv(tab2print, file = "tables/features_by_current_dx.csv")

## table one by gender
tab3 <- CreateTableOne(vars = myVars, data = active_ibd_subjects_meta,
                       factorVars = catVars, strata = "GENDERFLAG")

tab3print <- print(tab3, nonnormal = nonnormal, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
write.csv(tab3print, file = "tables/features_by_gender.csv")


# figures
## supplemental
### Cumulative Sum of ICD Diagnosis Codes
active_icd %>%
  mutate(dx_year = year(DIAGNOSIS_DATE)) %>%
  count(dx_year) %>%
  mutate(cum_sum = cumsum(n)) %>%
  ggplot(aes(x = as.factor(dx_year), y = cum_sum)) + 
  geom_col() + 
  theme_minimal() + 
  labs(x = "Year", y = "Count")

ggsave("figures/supp/cumulative_sum_of_icd_dx_codes.tiff",
       width = 20, height = 20, units = "cm")

### count of top 20 encounters for active subjectsnumber of encounters
active_encounters %>%
  count(ENC_TYPE) %>%
  slice_max(order_by = n, n = 20) %>%
  ggplot(aes(x = reorder(ENC_TYPE, -n), y = n)) + 
  geom_col() + 
  theme_minimal() + 
  coord_flip() + 
  labs(x = "Encounter Type", y = "Count")

ggsave("figures/supp/count_of_top_20_encounters_ehr_cohort.tiff",
       width = 20, height = 20, units = "cm")

### Distribution of Number of Unique Encounter Types Per Active Patient
active_ibd_subjects_meta %>%
  distinct(MRN, n_unique_encounters) %>%
  ggplot(aes(x = n_unique_encounters)) +
  geom_histogram(bins = 41) + 
  theme_minimal() + 
  labs(x = "n, Unique Encounter Types", y = "Count")

ggsave("figures/supp/distribution_of_n_unique_encounters.tiff",
       width = 20, height = 20, units = "cm")

### Distribution of Days of Folloup Duration
active_ibd_subjects_meta %>%
  distinct(MRN, follow_duration) %>%
  ggplot(aes(x = follow_duration)) +
  geom_histogram(bins = 40) +
  theme_minimal() + 
  labs(x = "n, Days of Follow-Up", y = "Count")

ggsave("figures/supp/distribution_of_days_of_follow_up.tiff",
       width = 20, height = 20, units = "cm")

### Distribution of Age
active_ibd_subjects_meta %>%
  filter(!is.na(age)) %>%
  distinct(MRN, age) %>%
  ggplot(aes(x = age)) +
  geom_histogram(bins = 30) +
  theme_minimal() + 
  labs(x = "n, Age as of 2021-12-31", y = "Count")

ggsave("figures/supp/distribution_of_ages.tiff",
       width = 20, height = 20, units = "cm")

active_ibd_subjects_meta %>%
  filter(!is.na(age)) %>%
  distinct(MRN, age, .keep_all = TRUE) %>%
  ggplot(aes(x = age, fill = GENDERFLAG)) +
  geom_histogram(bins = 30) +
  theme_minimal() + 
  scale_fill_viridis_d() + 
  labs(x = "n, Age as of 2021-12-31", y = "Count", fill = "Gender")

ggsave("figures/supp/distribution_of_ages_by_gender.tiff",
       width = 20, height = 20, units = "cm")

### Distribution of ICD Codes
active_ibd_subjects_meta %>%
  filter(!is.na(n_icd)) %>%
  distinct(MRN, n_icd) %>%
  ggplot(aes(x = n_icd)) +
  geom_histogram(bins = 30) +
  theme_minimal() + 
  labs(x = "n, ICD Codes", y = "Count")

ggsave("figures/supp/distribution_of_icd_codes.tiff",
       width = 20, height = 20, units = "cm")

### Distribution of Unique ICD Codes
active_ibd_subjects_meta %>%
  filter(!is.na(n_unique_icd)) %>%
  distinct(MRN, n_unique_icd) %>%
  ggplot(aes(x = n_unique_icd)) +
  geom_histogram(bins = 30) +
  theme_minimal() + 
  labs(x = "n, Unique ICD Codes", y = "Count")

ggsave("figures/supp/distribution_of_unique_icd_codes.tiff",
       width = 20, height = 20, units = "cm")


### Count of CCS Chapters
active_icd_ccs_mapped %>%
  distinct(MRN, ICD.10.CM.Diagnosis.Chapter) %>%
  count(ICD.10.CM.Diagnosis.Chapter) %>%
  ggplot(aes(x = reorder(ICD.10.CM.Diagnosis.Chapter, n), y = n)) + 
  geom_col() + 
  theme_minimal() + 
  coord_flip() + 
  labs(x = "CCS Diagnosis Chapter Type", y = "Count")

ggsave("figures/supp/count_of_unique_ccs_chapters.tiff",
       width = 20, height = 20, units = "cm")

### Count of Top 20 CCS Categories
active_icd_ccs_mapped %>%
  distinct(MRN, X.CCSR.CATEGORY.1.DESCRIPTION.) %>%
  count(X.CCSR.CATEGORY.1.DESCRIPTION.) %>%
  slice_max(order_by = n, n = 20) %>%
  ggplot(aes(x = reorder(X.CCSR.CATEGORY.1.DESCRIPTION., n), y = n)) + 
  geom_col() + 
  theme_minimal() + 
  coord_flip() + 
  labs(x = "CCS Diagnosis Category Type", y = "Count")

ggsave("figures/supp/count_of_unique_ccs_categories.tiff",
       width = 20, height = 20, units = "cm")

### Distribution of Chapters
active_ibd_subjects_meta %>%
  filter(!is.na(n_unique_ccs_chapters)) %>%
  distinct(MRN, n_unique_ccs_chapters) %>%
  ggplot(aes(x = n_unique_ccs_chapters)) +
  geom_density() +
  theme_minimal() + 
  labs(x = "n, Unique CCS Chapters", y = "Density")

ggsave("figures/supp/density_distribution_of_unique_ccs_chapters.tiff",
       width = 20, height = 20, units = "cm")
### Distribution of Number of Categories
active_ibd_subjects_meta %>%
  filter(!is.na(n_unique_ccs_categories)) %>%
  distinct(MRN, n_unique_ccs_categories) %>%
  ggplot(aes(x = n_unique_ccs_categories)) +
  geom_density() +
  theme_minimal() + 
  labs(x = "n, Unique CCS Categories", y = "Density")

ggsave("figures/supp/density_distribution_of_unique_ccs_categories.tiff",
       width = 20, height = 20, units = "cm")

