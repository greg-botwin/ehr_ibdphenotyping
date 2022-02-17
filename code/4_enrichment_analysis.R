library(tidyverse)

active_ibd_subjects_meta <- read_csv("data/active_ibd_subjects_meta.csv")
active_icd_ccs_mapped <- read_csv("data/miriad_diagnosis_codes_active_subjects_ccs_mapped.csv")
active_icd_ccs_mapped_subj_meta <- active_icd_ccs_mapped %>%
  left_join(., active_ibd_subjects_meta, by ="MRN")

# figures
## supplemental
### prevalence of ccs chapters in active subjects
active_icd_ccs_mapped_subj_meta %>%
  mutate(total_n = length(unique(MRN))) %>%
  group_by(ICD.10.CM.Diagnosis.Chapter, total_n) %>%
  summarise(unique_n = length(unique(MRN))) %>%
  ungroup() %>%
  mutate(freq = unique_n/total_n) %>%
  filter(freq > 0.01) %>%
  ggplot(aes(x = reorder(ICD.10.CM.Diagnosis.Chapter, freq), y = freq)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() + 
  theme_minimal() + 
  theme(plot.title.position = "plot") +
  scale_fill_brewer(palette = "Paired") + 
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Prevalence", x = NULL)

ggsave("figures/supp/prevalence_ccs_chapters.tiff",
       width = 40, height = 20, units = "cm")

### prevalence of ccs chapters in active subjects by dx
active_icd_ccs_mapped_subj_meta %>%
  group_by(current_diagnosis) %>%
  mutate(total_n = length(unique(MRN))) %>%
  group_by(current_diagnosis, ICD.10.CM.Diagnosis.Chapter, total_n) %>%
  summarise(unique_n = length(unique(MRN))) %>%
  ungroup() %>%
  mutate(freq = unique_n/total_n) %>%
  group_by(ICD.10.CM.Diagnosis.Chapter) %>%
  filter(all(freq > 0.01)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(ICD.10.CM.Diagnosis.Chapter, freq), y = freq,
             fill = current_diagnosis)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() + 
  theme_minimal() + 
  theme(plot.title.position = "plot") +
  scale_fill_brewer(palette = "Paired") + 
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Prevalence", x = NULL, fill = "IBD Diagnosis")

ggsave("figures/supp/prevalence_ccs_chapters_by_ibd_dx.tiff",
       width = 40, height = 20, units = "cm")

### percent diff by ibd dx of ccs chapters prevalence
active_icd_ccs_mapped_subj_meta %>%
  group_by(current_diagnosis) %>%
  mutate(total_n = length(unique(MRN))) %>%
  group_by(current_diagnosis, ICD.10.CM.Diagnosis.Chapter, total_n) %>%
  summarise(unique_n = length(unique(MRN))) %>%
  ungroup() %>%
  mutate(freq = unique_n/total_n) %>%
  group_by(ICD.10.CM.Diagnosis.Chapter) %>%
  filter(all(freq > 0.01)) %>%
  ungroup() %>%
  select(current_diagnosis, ICD.10.CM.Diagnosis.Chapter, freq) %>%
  pivot_wider(names_from = current_diagnosis, values_from = freq) %>%
  mutate(diff_cd_uc = `Crohn's Disease` - `Ulcerative Colitis`) %>%
  ggplot(aes(x = diff_cd_uc, y = reorder(ICD.10.CM.Diagnosis.Chapter,diff_cd_uc),
             fill =  diff_cd_uc< 0)) +
  geom_col(show.legend = TRUE) +
  labs(x = "Percent Difference", y = NULL, fill = "") +
  scale_fill_brewer(palette = "Paired", labels = c("CD Bias", "UC Bias")) + 
  scale_x_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(legend.position="bottom")

ggsave("figures/supp/per_diff_ccs_chapters_by_ibd_dx.tff",
       width = 40, height = 20, units = "cm")

### prevalence of ccs chapters in active subjects by gender
active_icd_ccs_mapped_subj_meta %>%
  filter(!is.na(GENDERFLAG)) %>%
  group_by(GENDERFLAG) %>%
  mutate(total_n = length(unique(MRN))) %>%
  group_by(GENDERFLAG, ICD.10.CM.Diagnosis.Chapter, total_n) %>%
  summarise(unique_n = length(unique(MRN))) %>%
  ungroup() %>%
  mutate(freq = unique_n/total_n) %>%
  group_by(ICD.10.CM.Diagnosis.Chapter) %>%
  filter(all(freq > 0.01)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(ICD.10.CM.Diagnosis.Chapter, freq), y = freq,
             fill = GENDERFLAG)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() + 
  theme_minimal() + 
  theme(plot.title.position = "plot") +
  scale_fill_brewer(palette = "Pastel2", direction=1) + 
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Prevalence", x = NULL, fill = "Gender")

ggsave("figures/supp/prevalence_ccs_chapters_by_gender.tiff",
       width = 40, height = 20, units = "cm")

### percent diff by gender of ccs chapters prevalence
active_icd_ccs_mapped_subj_meta %>%
  filter(!is.na(GENDERFLAG)) %>%
  group_by(GENDERFLAG) %>%
  mutate(total_n = length(unique(MRN))) %>%
  group_by(GENDERFLAG, ICD.10.CM.Diagnosis.Chapter, total_n) %>%
  summarise(unique_n = length(unique(MRN))) %>%
  ungroup() %>%
  mutate(freq = unique_n/total_n) %>%
  group_by(ICD.10.CM.Diagnosis.Chapter) %>%
  filter(all(freq > 0.01)) %>%
  ungroup() %>%
  select(GENDERFLAG, ICD.10.CM.Diagnosis.Chapter, freq) %>%
  pivot_wider(names_from = GENDERFLAG, values_from = freq) %>%
  mutate(diff_cd_uc = `F` - `M`) %>%
  ggplot(aes(x = diff_cd_uc, y = reorder(ICD.10.CM.Diagnosis.Chapter,diff_cd_uc),
             fill =  diff_cd_uc< 0)) +
  geom_col(show.legend = TRUE) +
  labs(x = "Percent Difference", y = NULL, fill = "") +
  scale_fill_brewer(palette = "Pastel2", labels = c("F Bias", "M Bias")) + 
  scale_x_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(legend.position="bottom")

ggsave("figures/supp/per_diff_ccs_chapters_by_dx.tiff",
       width = 40, height = 20, units = "cm")

### prevalence of ccs categories in active subjects
active_icd_ccs_mapped_subj_meta %>%
  mutate(total_n = length(unique(MRN))) %>%
  group_by(X.CCSR.CATEGORY.1.DESCRIPTION., total_n) %>%
  summarise(unique_n = length(unique(MRN))) %>%
  ungroup() %>%
  mutate(freq = unique_n/total_n) %>%
  filter(freq > 0.01) %>%
  slice_max(order_by = freq, n =20) %>%
  ggplot(aes(x = reorder(X.CCSR.CATEGORY.1.DESCRIPTION., freq), y = freq)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() + 
  theme_minimal() + 
  theme(plot.title.position = "plot") +
  scale_fill_brewer(palette = "Paired") + 
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Prevalence", x = NULL)

ggsave("figures/supp/prevalence_ccs_categories.tiff",
       width = 40, height = 20, units = "cm")

### prevalence of ccs categories in active subjects by dx
active_icd_ccs_mapped_subj_meta %>%
  group_by(current_diagnosis) %>%
  mutate(total_n = length(unique(MRN))) %>%
  group_by(current_diagnosis, X.CCSR.CATEGORY.1.DESCRIPTION., total_n) %>%
  summarise(unique_n = length(unique(MRN))) %>%
  ungroup() %>%
  mutate(freq = unique_n/total_n) %>%
  group_by(X.CCSR.CATEGORY.1.DESCRIPTION.) %>%
  filter(all(freq > 0.01)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(X.CCSR.CATEGORY.1.DESCRIPTION., freq), y = freq,
             fill = current_diagnosis)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() + 
  theme_minimal() + 
  theme(plot.title.position = "plot") +
  scale_fill_brewer(palette = "Paired") + 
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Prevalence", x = NULL, fill = "IBD Diagnosis")

ggsave("figures/supp/prevalence_ccs_chapters_by_ibd_dx.tiff",
       width = 40, height = 20, units = "cm")

### percent diff by ibd dx of ccs chapters prevalence
active_icd_ccs_mapped_subj_meta %>%
  group_by(current_diagnosis) %>%
  mutate(total_n = length(unique(MRN))) %>%
  group_by(current_diagnosis, ICD.10.CM.Diagnosis.Chapter, total_n) %>%
  summarise(unique_n = length(unique(MRN))) %>%
  ungroup() %>%
  mutate(freq = unique_n/total_n) %>%
  group_by(ICD.10.CM.Diagnosis.Chapter) %>%
  filter(all(freq > 0.01)) %>%
  ungroup() %>%
  select(current_diagnosis, ICD.10.CM.Diagnosis.Chapter, freq) %>%
  pivot_wider(names_from = current_diagnosis, values_from = freq) %>%
  mutate(diff_cd_uc = `Crohn's Disease` - `Ulcerative Colitis`) %>%
  ggplot(aes(x = diff_cd_uc, y = reorder(ICD.10.CM.Diagnosis.Chapter,diff_cd_uc),
             fill =  diff_cd_uc< 0)) +
  geom_col(show.legend = TRUE) +
  labs(x = "Percent Difference", y = NULL, fill = "") +
  scale_fill_brewer(palette = "Paired", labels = c("CD Bias", "UC Bias")) + 
  scale_x_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(legend.position="bottom")

ggsave("figures/supp/per_diff_ccs_chapters_by_ibd_dx.tff",
       width = 40, height = 20, units = "cm")

### prevalence of ccs chapters in active subjects by gender
active_icd_ccs_mapped_subj_meta %>%
  filter(!is.na(GENDERFLAG)) %>%
  group_by(GENDERFLAG) %>%
  mutate(total_n = length(unique(MRN))) %>%
  group_by(GENDERFLAG, ICD.10.CM.Diagnosis.Chapter, total_n) %>%
  summarise(unique_n = length(unique(MRN))) %>%
  ungroup() %>%
  mutate(freq = unique_n/total_n) %>%
  group_by(ICD.10.CM.Diagnosis.Chapter) %>%
  filter(all(freq > 0.01)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(ICD.10.CM.Diagnosis.Chapter, freq), y = freq,
             fill = GENDERFLAG)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() + 
  theme_minimal() + 
  theme(plot.title.position = "plot") +
  scale_fill_brewer(palette = "Pastel2", direction=1) + 
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Prevalence", x = NULL, fill = "Gender")

ggsave("figures/supp/prevalence_ccs_chapters_by_gender.tiff",
       width = 40, height = 20, units = "cm")

### percent diff by gender of ccs chapters prevalence
active_icd_ccs_mapped_subj_meta %>%
  filter(!is.na(GENDERFLAG)) %>%
  group_by(GENDERFLAG) %>%
  mutate(total_n = length(unique(MRN))) %>%
  group_by(GENDERFLAG, ICD.10.CM.Diagnosis.Chapter, total_n) %>%
  summarise(unique_n = length(unique(MRN))) %>%
  ungroup() %>%
  mutate(freq = unique_n/total_n) %>%
  group_by(ICD.10.CM.Diagnosis.Chapter) %>%
  filter(all(freq > 0.01)) %>%
  ungroup() %>%
  select(GENDERFLAG, ICD.10.CM.Diagnosis.Chapter, freq) %>%
  pivot_wider(names_from = GENDERFLAG, values_from = freq) %>%
  mutate(diff_cd_uc = `F` - `M`) %>%
  ggplot(aes(x = diff_cd_uc, y = reorder(ICD.10.CM.Diagnosis.Chapter,diff_cd_uc),
             fill =  diff_cd_uc< 0)) +
  geom_col(show.legend = TRUE) +
  labs(x = "Percent Difference", y = NULL, fill = "") +
  scale_fill_brewer(palette = "Pastel2", labels = c("F Bias", "M Bias")) + 
  scale_x_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(legend.position="bottom")

ggsave("figures/supp/per_diff_ccs_chapters_by_dx.tiff",
       width = 40, height = 20, units = "cm")

