library(tidyverse)
library(igraph)
library(widyr)
library(RCy3)

active_ibd_subjects_meta <- read_csv("data/active_ibd_subjects_meta.csv")
active_icd_ccs_mapped <- read_csv("data/miriad_diagnosis_codes_active_subjects_ccs_mapped.csv")

# create network file for cytoscape 
## ccs categories
### all subjects
active_icd_ccs_mapped %>%
  distinct(MRN, X.CCSR.CATEGORY.1.) %>%
  pairwise_count(item = X.CCSR.CATEGORY.1., feature = MRN) %>%
  graph_from_data_frame() %>%
  write.graph(file = "data/networks/all_ibd_categories.txt", format = "edgelist")

createNetworkFromIgraph(ig,"myIgraph")
g <- make_ring(10)
write_graph(g, "test.txt", "edgelist")  
### cd
### uc
### male
### female

df %>%
  distinct(MRN, ICD10_CODE) %>%
  filter(!is.na(ICD10_CODE)) %>%
  pairwise_count(item = ICD10_CODE, feature = MRN) %>%
  filter(n >= 1000) %>%
  graph_from_data_frame() 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()
