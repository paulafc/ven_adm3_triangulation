

# calculate_mode ----------------------------------------------------------
# Calculate mode
# If there is tie, concatenate all options
calculate_mode <- function(data, variable){
  data <- data |>
    group_by(across(all_of(admin.levels))) |>
    summarise(
      # Mode calculation
      !!sym(variable) := {
        # Handle case where the column has only NA values
        if (all(is.na(!!sym(variable)))) {
          no_info
        } else {
          modes <- get_modes(!!sym(variable))

          # Check if there's a tie in the mode
          if (length(modes) > 1) {
            # If tied modes, prepend "No hay consenso:"
            # paste(paste(modes, collapse = ","))
            paste("No hay consenso:", paste(modes, collapse = ","))
          } else {
            # If no tie, return the single mode
            modes
          }
        }
      },
      .groups = "drop"
    )
  return(data)
}  

# get_modes ---------------------------------------------------------------
# Function to calculate mode
get_modes <- function(variable_values) {
  # Calculate frequency table
  freq_table <- table(variable_values)
  
  # Find the maximum frequency
  max_freq <- max(freq_table)
  
  # Get the modes (values with the maximum frequency)
  modes <- names(freq_table[freq_table == max_freq])
  
  return(modes)
}


# peor_caso ---------------------------------------------------------------
# If any value matches var_choice[1], return var_choice[1]
# If any value matches var_choice[2], return var_choice[2]
# If all values are NA, return "No hay informacion"
# Otherwise, return the first value of the column (non-NA)
peor_caso <- function(data, variable, variable_choice){
  data <- data |>
    group_by(across(all_of(admin.levels))) |>
    summarise(
      !!sym(variable) := case_when(
        any(!!sym(variable) == variable_choice[1]) ~ variable_choice[1],  
        any(!!sym(variable) == variable_choice[2]) ~ variable_choice[2],  
        all(is.na(!!sym(variable))) ~ no_info,                            
        TRUE ~ first(!!sym(variable))                                     
      ),
      .groups = "drop"  # Drop grouping for cleaner output
    )  
  return(data)
}


# process_multiple_choice -------------------------------------------------
process_multiple_choice <- function(data, variable_multiple) {
  # select all relevant columns
  data <- data |>
    select(all_of(admin.levels), all_of(variable_multiple))
  
  # Agregate
  data <- data |>
    group_by(across(all_of(admin.levels))) |>
    summarize(across(all_of(variable_multiple), ~ min(sum(., na.rm = TRUE), 1), .names = "{.col}"), .groups = 'drop') 
  return(data)
} 


# multiple_choice_logic_check ---------------------------------------------
multiple_choice_logic_check <- function(data, variable_multiple, variable_priority, variable_ns_nr){
    # # Check logic Prioridad
    
    if(!is.na(variable_priority)){
      variable_priority <- names(data)[grepl(variable_priority, names(data))]
      cols <- setdiff(variable_multiple,c(variable_priority,variable_ns_nr))
      
      # Check "No barrier"
      # Set "no barrier" to 0 if barrier is reported, otherwise 1
      data <- data |>
        mutate(
          !!sym(variable_priority) := if_else(
            rowSums(select(data, all_of(cols)), na.rm = TRUE) > 0,
            0, # Set to 0 if sum is greater than 0
            1  # Set to 1 if sum is 0
          )
        )
    }
    
    # Check "Dont want to answer/dont know"
    if(!is.na(variable_ns_nr)){
      cols <- setdiff(variable_multiple,variable_ns_nr)
      
      data <- data |>
        mutate(
          !!sym(variable_ns_nr) := case_when(
            # If sum of selected columns > 0 and variable_ns_nr == 1, set to 0
            rowSums(select(data, all_of(cols)), na.rm = TRUE) > 0 & get(variable_ns_nr) == 1 ~ 0,
            
            # If sum of selected columns == 0 and variable_ns_nr == 1, set to 1
            rowSums(select(data, all_of(cols)), na.rm = TRUE) == 0 & get(variable_ns_nr) == 1 ~ 1,
            
            # If sum of selected columns including variable_ns_nr == 0, set to 0
            rowSums(select(data, c(all_of(cols), variable_ns_nr)), na.rm = TRUE) == 0 ~ 0,
            
            # Default case: retain the original value
            TRUE ~ get(variable_ns_nr)
          )
        )
      
    }
  
  data <- data |>
    mutate(
      across(
        c(all_of(cols), !!sym(variable_ns_nr)),
        ~ case_when(
          # If the sum of the selected columns equals 0, replace with "no_info"
          rowSums(select(data, c(all_of(cols), all_of(variable_ns_nr))), na.rm = TRUE) == 0 ~ NA_real_ ,
          TRUE ~ .
        )
      )
    )
  
      
  
    return(data)
  }


# concatenate_other_option ------------------------------------------------
# Concatenate the other options of the adm level
concatenate_other_option <- function(data, other_option){

  data <- data |>
    group_by(across(all_of(admin.levels))) |>
    summarize(
      !!sym(other_option) := if (all(is.na(!!sym(other_option)))) {
        NA_character_
      } else {
        paste(sort(unique(na.omit(!!sym(other_option)))), collapse = ", ")
      },
      .groups = "drop"
    )
  return(data)
}

# calculate_mean ----------------------------------------------------------
calculate_mean <- function(data, variable){
  data <- data |> 
    group_by(across(all_of(admin.levels))) |>
    summarise(!!sym(variable):= floor(mean(!!sym(variable), na.rm = TRUE)))
  return(data)
}

# necesidades_rank --------------------------------------------------------

necesidades_rank <- function(data, variable, points){

  # Pivot longer dataframe
  res <- data |>
    pivot_longer(cols = all_of(variable),
                 names_to = "priority",
                 values_to = "sector") 
  
  # Calculate total points for each sector
  res <- res  |> 
    mutate(points = points[priority]) |>
    group_by(across(all_of(admin.levels)), sector) |>
    summarize(total_points = sum(points, na.rm = TRUE), .groups = "drop") %>%  
    mutate(total_points = if_else(is.na(sector),0,total_points))
  
  # Concatenate sectors priority needs in case of ties
  res <- res |> 
    group_by(across(all_of(admin.levels)), total_points) |>
    mutate(sector = if_else(!is.na(sector),paste(sector, collapse = ", "),sector)) |>
    ungroup() |> 
    distinct()
  
  # Rank sectors by total points (select top 3) and create priority column
  res <- res |>
    group_by(across(all_of(admin.levels))) |>
    slice_max(total_points, n = 3, with_ties = TRUE) |>
    mutate(priority = case_when(
      row_number() == 1 ~ variable[1],
      row_number() == 2 ~ variable[2],
      row_number() == 3 ~ variable[3],
      TRUE ~ NA_character_
    )) |>
    complete(priority = variable, fill = list(priority = NA_character_))|>
    ungroup()
  

  # Pivot the data to wide format
  res_w <- res |>
    select(-total_points) |>
    pivot_wider(
      names_from = priority,  
      values_from = sector,   
      values_fill = list(sector = NA_character_) # Fill if "No ya informacion"
      # values_fn = ~paste(unique(.), collapse = ", "),  # Concatenate sectors if there are ties
      # values_fill = list(sector = NA_character_)  # Fill missing values with NA
    )
  
  # add No hay informacion if
  # 1st, 2nd and 3rd, or 2nd and 3rd are NA
  res_w <- res_w |>
    mutate(across(all_of(c(variable[1], variable[2], variable[3])), 
                  ~ case_when(
                    is.na(get(variable[1])) & is.na(get(variable[2])) & is.na(get(variable[3])) ~ no_info,
                    TRUE ~ .)))|>
  mutate(across(all_of(c(variable[2], variable[3])), 
                ~ case_when(
                  is.na(get(variable[2])) & is.na(get(variable[3])) ~ no_info,
                  TRUE ~ .)))
    
  return(res_w)
}



# resolve_ties ------------------------------------------------------------
# Resolve mode ties using variable_choice order

resolve_ties <- function(modes, var_choice) {
  # If there are multiple modes, select the one that appears first in var_choice
  if (length(modes) > 1) {
    preferred_mode <- intersect(var_choice, modes)[1]
    return(preferred_mode)
  } else {
    # If no tie, return the single mode
    return(modes[1])
  }
}


# moda_y_peorcaso ---------------------------------------------------------
moda_y_peorcaso <- function(data, variable, variable_choice) {
  data <- data %>%
    group_by(across(all_of(admin.levels))) %>%
    summarise(
      # Mode calculation
      !!sym(variable) := {
        # Step 1: Check if all values are NA
        if (all(is.na(!!sym(variable)))) {
          no_info  # If all values are NA, return "No hay informacion"
        } else {
          # Step 2: Calculate modes
          modes <- get_modes(!!sym(variable))
          
          # Step 3: Resolve ties if multiple modes exist
          if (length(modes) > 1) {
            resolve_ties(modes, variable_choice)
          } else {
            modes
          }
        }
      },
      .groups = "drop"  # Drop grouping for cleaner output
    )
  
  return(data) 
}

replace_labels <- function(sv, ch, data) {
  # Prepare the survey (sv) dataframe
  # Prepare the survey (sv) dataframe
  sv <- sv %>%
    select(type, name) %>%
    filter(str_detect(type, "select_")) %>%
    filter(!str_detect(type, "admin")) %>%
    separate(type, into = c("type", "choice"), sep = " ", extra = "merge", fill = "right")
  
  # Prepare the choice mapping (ch) dataframe
  ch <- ch %>%
    filter(!str_starts(list_name, "admin")) %>%
    rename_with(~ "label", starts_with("label")) %>% 
    select(list_name, name, label) %>% 
    mutate(label=if_else(name==no_info, "No hay información",label))
  
  # Process 'select_multiple' columns
  check_multiple <- sv %>%
    filter(type == "select_multiple")
  
  if (nrow(check_multiple) > 0) {
    col_names <- check_multiple$name
    col_names <- colnames(data)[Reduce(`|`, lapply(col_names, function(x) startsWith(colnames(data), paste0(x, '/'))))]
    
    # Replace values for 'select_multiple' columns
    data <- data %>%
      mutate(across(
        all_of(col_names),
        ~ case_when(
          . == 1 ~ "Sí",
          . == 0 ~ "No",
          is.na(.) ~ NA_character_,
          TRUE ~ as.character(.)
        )
      ))
  }
  
  # Process 'select_one' columns
  check_one <- sv %>%
    # filter(type == "select_one") %>%
    filter(name %in% intersect(sv$name, colnames(data)))
  
  if (nrow(check_one) > 0) {
    data <- data %>%
      mutate(across(
        all_of(check_one$name),
        ~ sapply(., function(x) {
          # Check if "No hay consenso" is present in the string
          has_no_hay_consenso <- grepl("^No hay consenso:", x)
          
          # Extract the part after "No hay consenso:" if it exists
          if (has_no_hay_consenso) {
            prefix <- "No hay consenso: "
            values_part <- sub("^No hay consenso: ", "", x)
          } else {
            prefix <- ""
            values_part <- x
          }
          
          # Split the concatenated values in each cell
          split_values <- unlist(strsplit(values_part, ","))
          split_values <- trimws(split_values)
          
          # Get the replacement mapping for each 'choice'
          replacement_map <- ch %>%
            filter(list_name %in% check_one$choice) %>%
            select(name, label)
          
          # Replace values in each split value using the replacement map
          replaced_values <- ifelse(
            split_values %in% replacement_map$name,
            replacement_map$label[match(split_values, replacement_map$name)],
            NA_character_
          )
          
          # If all replaced values are NA, return NA_character_
          if (all(is.na(replaced_values))) {
            NA_character_
          } else {
            # Rejoin the replaced values back into a single string, preserving the prefix
            paste0(prefix, paste(replaced_values[!is.na(replaced_values)], collapse = ", "))
          }
        })
      ))
  }
  
  return(data)
}


# kobo_relevant_to_r ------------------------------------------------------
# transform kobo survey relevant column into R condition
kobo_relevant_to_r <- Vectorize(function(input_str) {
  # Remove leading and trailing spaces
  input_str <- trimws(input_str)
  
  # Define the pattern for matching 'selected' expressions
  selected_pattern <- "selected\\(\\$\\{([a-zA-Z0-9_]+)\\},\\s*\"?([a-zA-Z0-9_\\-]+)\"?\\)"
  
  # Process 'selected' expressions first to convert them to the required format
  input_str <- gsub(selected_pattern, "\\1 == \"\\2\"", input_str)
  
  # Handle 'not' negation, apply it to the expression, and convert 'and'/'or' to '&'/'|'
  input_str <- gsub("not\\((.*?)\\)", "!(\\1)", input_str)
  input_str <- gsub(" and ", " & ", input_str)
  input_str <- gsub(" or ", " | ", input_str)
  
  return(input_str)
})


# kobo_logical_checks -----------------------------------------------------
kobo_logical_checks<-function(df, sv){
  
  df_conditions <- sv |>
    select(type,name,relevant)|>
    filter(!is.na(relevant))
  
  df_conditions <- df_conditions |>
    mutate(revR = kobo_relevant_to_r(relevant)) |>
    filter(sapply(name, function(n) any(grepl(n, colnames(merged_df))))) |>
    filter(type!="text")|>
    separate(type, into = c("type", "choice"), sep = " ", extra = "merge", fill = "right") 
  # mutate(name= if_else(type=="select_multiple",paste0(name,"/"),name))
  
  # create duplicated logical checks for select_multiple options
  df_conditions <- df_conditions %>%
    mutate(
      # Create a new column for modified names when type == "select_multiple"
      new_name = if_else(type == "select_multiple", paste0(name, "/"), NA_character_)
    ) %>%
    # Duplicate rows where type == "select_multiple" and keep the original
    bind_rows(
      filter(., type == "select_multiple") %>%
        mutate(name = new_name)
    ) %>%
    # Remove the helper column `new_name`
    select(-new_name)
  
  for (i in 1:nrow(df_conditions)) {
    
    # Extract the relevant columns from the tibble
    condition_column <- df_conditions$name[i]
    condition <- df_conditions$revR[[i]]
    type <- df_conditions$type[[i]]
    print(paste0(type," - ",condition_column,": ",i))
    
    # Get the column(s) matching the pattern from df (multiple selection)
    if (type == "select_multiple" & grepl("/",condition_column)) {
      matching_columns <- df %>%
        select(all_of(matches(condition_column)))
    }
    
    else {
      matching_columns <- df %>%
        select(all_of(condition_column))  
    }
    # Ensure we only work with the unique type of the first matching column
    column_type <- typeof(matching_columns[[1]])  
    
    if (column_type == "double") {
      # Convert matching columns to numeric (if they're not already numeric)
      df <- df %>%
        mutate(across(matches(condition_column), ~ as.numeric(.)))
      df <- df %>%
        mutate(across(matches(condition_column),  
                      ~ if_else(
                        eval(parse(text = condition)),  
                        .,  NA_real_)
        )
        )
      
    } else{
      df <- df %>%
        mutate(across(condition_column,  # Select columns matching condition_column pattern
                      ~ if_else(
                        eval(parse(text = condition)),  # Evaluate the condition
                        .,  NA_character_)
        )
        )
    }
  }
  return(df)
}
