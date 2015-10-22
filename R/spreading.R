





spread_untidy <- function(data) {


  tracking_env <- attr(data, "tracking_env")

  if (!is_sth_a(tracking_env, "class", "non_tidy")) {
    return(list(data))
  } else {

    key_to_non_tidy  <- tracking_env[["key_var_data"]][["key"]]
    non_tidy_colname <- tracking_env[["key_var_data"]][["the_non_tidy_col"]]
    non_tidy_data    <- tracking_env[["key_var_data"]][["data"]]

    data[[non_tidy_colname]] <- as.character(non_tidy_data)
    data <- project_id_finder(data)


    if (is_sth_a(tracking_env,"class", "project_id")){
      project_id <- tracking_env[["project_id"]][["project_id"]]
      # redundant_cols_data <- tracking_env[["redundant_cols"]][["redundant_cols_data"]]
      # redundant_cols_classes <- tracking_env[["redundant_cols"]][["redundant_cols_classes"]]
    }
    if (!is_sth_a(tracking_env,"class", "project_id")) {
      project_id <- "project_id"
      data[["project_id"]] <- 1
    }


    return(

    lapply(levels(data[[project_id]]), function(x) {
        subdat <- data[data[[project_id]]==x, ]

        spread_out <-  tryCatch(

          subdat  %>%
            tidyr::spread_(key_to_non_tidy, non_tidy_colname
                           , convert=FALSE)
          ,

          # if duplicate row error than concantenate the offending rows
          error = function(e)
          {
            select_all_indices <- lapply(
              strsplit(
                gsub("[ ()]", "", strsplit(strsplit(e$message, "rows ")[[1]][2], "),")[[1]])
                , ",\\s*")
              , as.numeric)

            subdat[sapply(select_all_indices, "[", 1), non_tidy_colname] <- sapply(select_all_indices, function(i) {
              paste(subdat[[non_tidy_colname]][i], collapse = ", ")
            })


            subdat[setdiff(1:nrow(subdat),
                           setdiff(
                             unlist(select_all_indices),
                             sapply(select_all_indices, "[", 1)
                           )), ] -> non_duplicates_data


            non_duplicates_data %>%
              tidyr::spread_(key_to_non_tidy, non_tidy_colname
                             , convert=FALSE)
          } # end of error
        ) # end of tryCatch


        if (is_sth_a(tracking_env,"class", "project_id")) {
          spread_out <- one_answer_cols(spread_out, "project_id_cols")

          # once we go wide, we need to re-assign levels to factors
          spread_out <- strip_levels(spread_out)

          spread_out[sapply(spread_out, is.character)] <- post_parse_dat(spread_out[sapply(spread_out, is.character)])
          # grp_sprd_to_int(spread_out)
          assign_env(spread_out, tracking_env)

        } else {
          assign_env(spread_out, tracking_env)
      }
        })

    ) # end of return
  } # end of else
} # end of spread_untidy




##########################################
#        Post-Spread_Untidy Clean Up
##########################################


grp_sprd_to_int <- function(data) {

  . <- NULL

  tracking_env <- attr(data, "tracking_env")

  names_of_factor_vars <- colnames(data[sapply(data, is.factor)])

  lapply(data[sapply(data, is.factor)], as.integer) %>%
    as.data.frame() %>%
    stats::setNames(.,names_of_factor_vars) -> data[sapply(data, is.factor)]

  assign_env(data, tracking_env)
}


post_convert_factor <- function(text_vec, perc_unique=0.98) {

  text_vec[is.na(text_vec)] <- "UNDERCOVER_NA"
  text_vec

  na_test <- is.na(suppressWarnings(as.numeric(text_vec)))

  if (all(na_test) ) {

    if (length(unique(na.omit(text_vec)))/length(na.omit(text_vec)) > perc_unique) {
      return(text_vec)
      # for non-repitition of categories
      # if (max(nchar(text_vec)) > max_char ) return(text_vec)
    } else {
      factor(text_vec)
    }

  } else if (mean(na_test)<=.05) {
    return(text_vec)
  }
}

# parse data types and convert to numeric
post_parse_dat <- function(orig_dat) {

  orig_dat[] <- lapply(orig_dat, convert_date)

  orig_dat[] <- lapply(orig_dat, convert_logical)
  orig_dat[] <- lapply(orig_dat, post_convert_factor)

  orig_dat[] <- lapply(orig_dat, convert_numeric)
  orig_dat[] <- lapply(orig_dat, convert_integer)
  orig_dat
}


strip_levels <- function(dat){
  dat[sapply(dat, is.factor)] <- lapply(dat[sapply(dat, is.factor)], factor)
  dat
}

