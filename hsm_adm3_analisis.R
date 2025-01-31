# ---------------------------------------------------------------------------------------------------------
# HSM VENEZUELA ANALYSIS SCRIPT 
# Author: Paula Costa
# ---------------------------------------------------------------------------------------------------------

# Clears all objects from the workspace
rm(list=ls(all=T))

# Install an load all required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, readxl, openxlsx, purrr)
options(dplyr.summarise.inform = FALSE)

# set working directory to this script's locations: no need to check the file path manually
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

source("Utils.R")

# INPUT/OUTPUT DATA -------------------------------------------------------
# Change accordingly
file.data <- "HSM_Noviembre2024_cleaned_dataset"
file.kobo <- "VEN_HSM_Noviembre2024_kobo"
file.rules <- "VEN_HSM_Noviembre2024_reglas_parroquias"
file.output <- "2025_03_hsm_adm3_analysis"


# VARIABLES ---------------------------------------------------------------
# Administrative level labels
admin.levels <- c("admin1_code", "admin1_label", "admin2_code", "admin2_label", "admin3_code", "admin3_label")
no_info <- "ns_nr"


# READ DATA ---------------------------------------------------------------
data <- read_excel(paste0("input/",file.data,".xlsx"))
sv <- read_excel(paste0("input/",file.kobo,".xlsx"), sheet = "survey")  
ch <- read_excel(paste0("input/",file.kobo,".xlsx"), sheet = "choices")  
data.rules <- read_excel(paste0("input/",file.rules,".xlsx"), sheet = "Clasificación de reglas")


# ANALYSIS ----------------------------------------------------------------

result <- lapply(1:length(data.rules$variable), function(i){
  # Dynamic variables
  var <- trimws(unlist(strsplit(data.rules$variable[i], ";")))
  var_choice <- trimws(unlist(strsplit(data.rules$orden_prioridad[i], ";")))
  exclude_choices <- trimws(unlist(strsplit(data.rules$opcion_excluir[i], ";")))
  regla <- data.rules$regla[i]
  
  # print operation process
  print(paste0(i,"_", all_of(var),": regla ",regla))
  
  # Select variable column, change exlcude_choice to NA
  df <- data |>
    select(all_of(admin.levels), var) |> 
    mutate(across(all_of(var), ~ ifelse(. %in% exclude_choices, NA, .)))

  # Check rules
  if (regla == 1) {
    result <- calculate_mode(df, var)
  }

  if (regla == 2) {
    result <- peor_caso(df, var, var_choice)
  }

  if (regla == 3) {
    df <- data
    
    # multiple choice variables
    var_multiple_cols <- colnames(df)[startsWith(colnames(df), paste0(var,'/'))]
    result <- process_multiple_choice(data = df, variable_multiple = var_multiple_cols)
    
    # Logical check
    var_choice <- unique(names(result)[grepl(var_choice, names(result))])
    exclude_choices <- unique(names(result)[grepl(exclude_choices, names(result))])
    result <- multiple_choice_logic_check(result,var_multiple_cols,var_choice, exclude_choices)
    
    # concatenate results to variable column (multiple choice summary)
    result <- result %>%
      mutate(!!sym(var) := apply(result[var_multiple_cols], 1, function(x) {
          if (all(is.na(x))) {
            NA_character_
          } else {
            parts <- names(x)[x == 1]
            paste(sub(".*?/", "", parts), collapse = ",")
          }
        }),.after = admin.levels)
  }
  
  if (regla == 4) {
      result <- concatenate_other_option(df, var)
  }
  
  if (regla == 5) {
    # Convert variable to numbers
    seguridad_n <- setNames(c(length(var_choice):1), var_choice)
    df <- df |>
      mutate(seguridad = case_when(
        !!sym(var) %in% names(seguridad_n) ~ map_dbl(names(seguridad_n), ~ seguridad_n[.x])[match(!!sym(var), names(seguridad_n))],
        TRUE ~ NA_real_))
    
    result <- calculate_mean(df, var)
    
    # Covert back to variable name
    result <- result |>
      mutate(seguridad = case_when(
        !!sym(var) %in% seguridad_n ~ names(seguridad_n)[match(!!sym(var), seguridad_n)],
        TRUE ~ NA_character_))
    }

  if (regla == 6) {
    # Define the point values
    points <- setNames(c(8, 6, 3), var)
    result <- necesidades_rank(data = df, variable = var, points = points)
  }
  
  if (regla == 7) {
    result <- moda_y_peorcaso(df, var, var_choice)
  }
  return(result)
    
})

# MERGE INDICATORS --------------------------------------------------------
merged_df <- reduce(result, left_join, by = admin.levels)


# ORGANISE COLUMN ORDER ---------------------------------------------------
col.order <- intersect(colnames(data), colnames(merged_df))
merged_df <- merged_df |> select(all_of(col.order))


# LOGICAL CHECKS - KOBO ---------------------------------------------------
kobo_relevant <- sv |>
  select(type,name,relevant)|>
  filter(!is.na(relevant))|>
  mutate(revR = kobo_relevant_to_r(relevant))

# clean data based on kobo relevant
merged_df <- kobo_logical_checks(merged_df, sv)


# CHANGE LABELS -----------------------------------------------------------
label_df <- replace_labels(sv, ch, merged_df)
# t <- mutate(res,across(everything(), ~ ifelse(is.na(.), "N/A", .)))

# EXPORT ------------------------------------------------------------------
write.xlsx(kobo_relevant,paste0("output/kobo_relevant.xlsx"))
write.xlsx(merged_df,paste0("output/",file.output,".xlsx"))
write.xlsx(label_df,paste0("output/",file.output,"_label.xlsx"))
  
