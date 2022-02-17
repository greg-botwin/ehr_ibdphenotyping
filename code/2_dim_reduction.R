library(tidyverse)
library(corrr)
active_ibd_subjects_meta <- read_csv("data/active_ibd_subjects_meta.csv")
active_icd_ccs_mapped <- read_csv("data/miriad_diagnosis_codes_active_subjects_ccs_mapped.csv")

# create matrix 1 presence of icd code 0 absence of common code
# present in >= 10 subjects
common_codes <- active_icd_ccs_mapped %>%
  group_by(ICD10_CODE) %>%
  summarise(n_subjects = length(unique(MRN))) %>%
  filter(n_subjects >= 10)

icd_matrix <- active_icd_ccs_mapped %>%
  filter(ICD10_CODE %in% common_codes$ICD10_CODE) %>%
  distinct(MRN, ICD10_CODE) %>%
  mutate(present = 1) %>%
  pivot_wider(names_from = ICD10_CODE, values_from = present, values_fill = 0)

dim(icd_matrix)
# 4416 subjects and 1213 icd codes

# perform pca unscaled, uncentered
set.seed(1234)
pr.out <- icd_matrix %>%
  select(-MRN) %>%
  prcomp()

# select top 5 pcs and add back meta values
pr1t5 <- pr.out$x %>%
  as_tibble() %>%
  select(1:5)

pr1t5 <- pr1t5 %>%
  mutate(MRN = icd_matrix$MRN) %>%
  left_join(., active_ibd_subjects_meta, by = "MRN")

# figures
## principal component analysis
### colored by current diagnosis
pr1t5 %>%
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point(aes(color = current_diagnosis)) + 
  theme_minimal() + 
  labs(color = "IBD Diagnosis") +
  scale_colour_viridis_d(option = "viridis")

ggsave("figures/fig_1a_principal_component_analysis_of_icd_matrix_by_ibd_dx.tiff",
       width = 20, height = 20, units = "cm")

### by gender
pr1t5 %>%
  filter(!is.na(GENDERFLAG)) %>%
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point(aes(color = GENDERFLAG)) + 
  theme_minimal()  + 
  labs(color = "Gender") +
  scale_colour_viridis_d(option = "plasma")


ggsave("figures/fig_1b_principal_component_analysis_of_icd_matrix_by_gender.tiff",
       width = 20, height = 20, units = "cm")

### by age
pr1t5 %>%
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point(aes(color = age)) + 
  theme_minimal() + 
  labs(color = "Age") +
  scale_colour_viridis_c()

ggsave("figures/fig_1c_principal_component_analysis_of_icd_matrix_by_age.tiff",
       width = 20, height = 20, units = "cm")

### by duration of follow up
pr1t5 %>%
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point(aes(color = follow_duration)) + 
  theme_minimal() +
  labs(color = "Days of Follow-Up")

ggsave("figures/fig_1d_principal_component_analysis_of_icd_matrix_by_days_of_fu.tiff",
       width = 20, height = 20, units = "cm")

### by n unique encounters
pr1t5 %>%
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point(aes(color = n_unique_encounters)) + 
  theme_minimal() +
  labs(color = "n Unique Encounter Types") +
  scale_colour_viridis_c(option = "magma")

ggsave("figures/fig_1e_principal_component_analysis_of_icd_matrix_by_n_encounters.tiff",
       width = 20, height = 20, units = "cm")

### correlation between pca and subejct level features

pr1t5 %>%
  select(PC1, PC2, PC3, PC4, PC5, n_unique_encounters,
         age, follow_duration) %>%
  rename(Age = age,
         "n Unique Encounters" = n_unique_encounters,
         "Follow Up Duration" = follow_duration) %>%
  correlate(method = "spearman", use="complete") %>% 
  network_plot(min_cor = .2, colours = c("indianred2", "black", "skyblue1"),
               repel = TRUE, curved = FALSE,) 

ggsave("figures/fig_1f_network_plot_of_correlation_btwn_pcs_and_subject_features.tiff",
       width = 20, height = 20, units = "cm")

## supplemental 
### percent variation explained by principal component and cummulative PVE
pve <- 100 * pr.out$sdev^2 / sum (pr.out$sdev^2)
par (mfrow = c(1, 2))
plot (pve , type = "o", ylab = "PVE ",
      xlab = " Principal Component ", col = " blue ")
plot ( cumsum (pve), type = "o", ylab = " Cumulative PVE ",
       xlab = " Principal Component ", col = " brown3 ")

ggsave("figures/supp/pve_and_cumulative_pve_by_pc.tiff",
       width = 20, height = 20, units = "cm")

### principal component analysis of icd matrix not categorized
pr1t5 %>%
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point() + 
  theme_minimal()

ggsave("figures/supp/principal_component_analysis_of_icd_matrix_uncategorized.tiff",
       width = 20, height = 20, units = "cm")
