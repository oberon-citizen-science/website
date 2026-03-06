library(tidyverse)
library(iucnredlist)

# Initialize the API with your API key
api <- init_api("wR2AEngF9EFEj7omduXw28yjzxJ2rFCvmQfS")

australia_iucn_assessments <- assessments_by_group(api,
  group = "countries",
  code = "AU",
  year_published = NULL,
  latest = TRUE,
  scope_code = 1,
  wait_time = 0.5,
  show_warnings = TRUE)

save(australia_iucn_assessments, file="assets/australia_iucn_assessments.RData")

# Pass assessment IDs into the assessment_data_many() function
australian_iucn_full_data <- assessment_data_many(api,
                                                  assessment_ids = australia_iucn_assessments$assessment_id)

save(australian_iucn_full_data, file="assets/australian_iucn_full_data.RData")

red_list_categories <- extract_element(australian_iucn_full_data, "red_list_category")
taxons <- extract_element(australian_iucn_full_data, "taxon")
taxon_synonyms <- extract_element(australian_iucn_full_data, "taxon_synonyms")
taxon_common_names <- extract_element(australian_iucn_full_data, "taxon_common_names")

red_list_synonyms <- taxons |>
              left_join(taxon_synonyms, by=join_by(assessment_id)) |>
              filter(!is.na(genus_name.y) & !is.na(species_name.y)) |>
              rename(genus_name = genus_name.y,
                     species_name = species_name.y) |>
              select(assessment_id, genus_name, species_name) |>
              select(assessment_id, genus_name, species_name)

red_list_taxons <- taxons |>
              select(assessment_id, scientific_name, genus_name, species_name) |>
              select(assessment_id, genus_name, species_name)

red_list <- red_list_taxons |>
              bind_rows(red_list_synonyms) |>
              distinct(assessment_id, genus_name, species_name) |>
              left_join(red_list_categories, by=join_by(assessment_id)) |>
              left_join(filter(taxon_common_names,main==TRUE, language == 'eng'),
                        by=join_by(assessment_id)) |>
              rename(common_name = name) |>
              select(assessment_id, genus_name, species_name, common_name,
                     code, description) |>
              rename(red_list_code = code,
                     red_list_category = description)

save(red_list, file="assets/red_list.RData")

red_list_filtered <- red_list |>
                      filter(red_list_code %in% c('CR', 'EN', 'NT', 'VU'))

save(red_list_filtered, file="assets/red_list_filtered.RData")



