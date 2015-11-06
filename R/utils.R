
###############################
#       MAIN PARSER
###############################
# parse data types and convert to numeric
parse_dat <- function(orig_dat) {

  # clean up import
  orig_dat[] <- lapply(orig_dat, remove_trailing_spaces)
  orig_dat    <- remove_white_space(orig_dat)
  orig_dat    <- remove_NA_cols(orig_dat)

  attr(orig_dat, "tracking_env") <- new.env(hash=FALSE)

  # beginning of environment for class tracking
  # do type conversions
  orig_dat[] <- lapply(orig_dat, convert_email)  # actual convert to factor happens in convert_factor
  orig_dat[] <- lapply(orig_dat, convert_guid)   # actual convert to factor happens in convert_factor

  orig_dat[] <- lapply(orig_dat, convert_date)
  orig_dat[] <- lapply(orig_dat, convert_logical)
  orig_dat[] <- lapply(orig_dat, convert_factor, orig_dat)

  # test to make sure not more than one untidy var
  # if so, stop and warning which vars are suspect
  if (sum(sapply(orig_dat, is, "non_tidy"))>1) {
    notify_about <- names(which(sapply(orig_dat, is, "non_tidy")))
    stop(warning(paste("multiple untidy columns detected @ \n\t",
                       paste(notify_about, collapse=", \n\t"),
                       "\nThis system can resolve at most one untidy column.")) )
  }


  orig_dat[] <- lapply(orig_dat, convert_numeric)
  orig_dat[] <- lapply(orig_dat, convert_integer)
  orig_dat
}








###############################
#       CONVERTERS
###############################


#test for whether to convert integer
convert_numeric <- function(text_vec) {


  # this handles any untidy vars that will be spread later
  if (is(text_vec, "non_tidy")) return(text_vec)

  # thus if it passes this test it can already be coerced and we don't need to do anything
  if (is.integer(text_vec)) return(text_vec)

  # if currently not class numeric
  if (!is.numeric(text_vec)) {
    # if not char, then doesn't need processing
    if (!is.character(text_vec) ) return(text_vec)
    # if it coerces to NA, then not possible to coerce to integer
    else if (all(is.na(suppressWarnings(as.numeric(text_vec))) ) ) return(text_vec)
    # testing for numeric saved as text
    else suppressWarnings(as.numeric(text_vec))
  } else {
    return(text_vec)
  }
}


convert_integer <- function(vec) {

  if (!is.numeric(vec)) return(vec)

  #  can a char vec coerced to numeric be divided by 1 without remainder?
  # If so, then TRUE and pass to as.integer in main function
  if (all(vec  %% 1 < 0.00000000000001, na.rm = TRUE )) {
    as.integer(vec)
  } else {
    return(vec)
  }
}


key_finder2 <- function(dat) {


  key_candidates <- colnames(dat[sapply(dat, function(x) length(unique(x))>1)])
  # eliminate cols with all unique answers
  key_candidates <- colnames(dat[sapply(dat[key_candidates], function(x) length(unique(x))<length(x))])
  # find col with highest ratio of lengths of rles to overall length
  # for a key the rle(x)$lengths should be 1 for each row,so it has same length (or close) to nrows in dat
  # consider names(which.max(sapply(dat, function(x) (1-length(unique(x))/length(x))*length(rle(x)$lengths)/ length(x))))

  key_candidates <- suppressWarnings(names(which.max(sapply(dat, function(x)  try(length(rle(x)$lengths)/ length(x), silent=TRUE) )) ))
  return(key_candidates[1])
}


convert_remaining_guid <- function(col){
  if (all(is_guid_element(na.omit(col))) ) {
    factor(col)
  } else {
    col
  }
}


convert_remaining_email <- function(col) {
  if (all(is_email_element(na.omit(col)))) {
    factor(col)
  } else {
    col
  }
}

remaining_guid_emails_to_factor <- function(dat){
  dat[] <- lapply(dat, convert_remaining_guid)
  dat[] <- lapply(dat, convert_remaining_email)
  dat
}



convert_factor <- function(text_vec, dat, perc_unique=0.98) {
  # , max_char = 150
  if (!is.character(text_vec)) return(text_vec)                    # testing for something saved as text

  if (is(text_vec, "email") | is(text_vec, "GUID") ){
    return(text_vec)
  }

  na_test <- is.na(suppressWarnings(as.numeric(na.omit(text_vec))))

  if ( all(na_test) ) {

    if (length(unique(na.omit(text_vec)))/length(na.omit(text_vec)) > perc_unique) {
      class(text_vec) <- c("open-text", class(text_vec))
        return(text_vec) #
      # for non-repitition of categories
      } else {
        factor(text_vec)
      }

  } else if (mean(na_test)<=.05) {
    return(text_vec)
    } else {
      key = key_finder2(dat)

      class(text_vec) <- c("non_tidy",class(text_vec))
      attr(text_vec, "key") <- key

      return(text_vec)
    }
}











convert_date <- function(text_vec){
  # vec of 100 to test for date_time
  first_100 <- na.omit(text_vec)[1:100]
  # if not character (which is what should be left
  # for date or time vars after being read in my read.csv)
  # return back as unparsed
  if(!is.character(first_100)) return(text_vec)

  # if any mon abbr or sequence of 4 or 2 digits are found
  if (all(grepl("(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)|(([-/]\\d{4}\\b)|(\\b\\d{4}[-/]))|\\b\\d{2}[/-]\\d{2}[/-]\\d{4}\\b", first_100, ignore.case=TRUE))) {

    # parse the text
    parsed_text_vec <- parsedate::parse_date(first_100)
    # if the parsed text parses to NA, then return text vec as not date or time
    if (all(is.na(parsed_text_vec))) return (text_vec)
  } else {
    return(text_vec)
  }

  # if parsed vec contains a ":" then it was date_time
  if (any(grepl("[:]",text_vec)))   {
    parsedate::parse_date(text_vec)
    # else it is just date
  } else as.Date(parsedate::parse_date(text_vec) )

}





convert_logical <- function(text_vec) {
  if (!all(grepl("^T|F|TRUE|FALSE|NA$", text_vec))) {
    return(text_vec) }
  else {
    as.logical(text_vec)
  }
}


is_guid_element <- function(x) {
  x <- gsub("^\\s+|\\s+$", "", x)
  grepl("\\d", x) &  grepl("^[A-Za-z0-9-]{5,}$", x)
}

convert_guid <- function(col){

  y <- na.omit(col)
  is_digit_guid <- all(grepl("^\\d{6,}$", y)) & sd(nchar(y)) == 0

  if (all(is_guid_element(y)) | is_digit_guid) {

  col <- as.character(col)
  col[is.na(col)] <- "UNDERCOVER_NA"
  col <- factor(col)
  class(col) <- c(class(col), "GUID")
  col
  } else {
    col
  }
}

is_email_element <- function(x) {
  grepl("([_+a-z0-9-]+(\\.[_+a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,14}))", x, ignore.case = TRUE)
}

convert_email <- function(col) {
  if (all(is_email_element(na.omit(col)))) {
    class(col) <- c("email", "character")
    col
  } else {
    col
  }

}





###############################
#        HANDLE TEXT
###############################


# non_text_indices <- function(orig_dat){
#   which(!is.na(colMeans(suppressWarnings(sapply(orig_dat, as.numeric)), na.rm = TRUE)))
# }

non_text_indices <- function(orig_dat){
    sort(unique(c(which(!is.na(colMeans(suppressWarnings(sapply(orig_dat, as.numeric)), na.rm = TRUE))),
                  which(sapply(orig_dat, is, "email")))))
}

## Not currently used
put_back_text <- function(sim_dat, orig_dat) {

  if (all(colnames(orig_dat) %in% colnames(sim_dat)))  return(sim_dat)

  data.frame(
    sim_dat,
    orig_dat[, setdiff(colnames(sim_dat), colnames(orig_dat)), drop=FALSE],
    stringsAsFactors = FALSE
  )

}




###############################
#        Environments
###############################


assign_env <- function(dat, env){
  attr(dat, "tracking_env") <- env
  return(dat)
}


## check if element exists in a class in an env
is_sth_a <- function(env, class, element) {

  any(env[[class]] %in% element)
}

## check if an named element exists in a class in env
exists_in_env <- function(env, class, element = NULL) {
  if (!is.null(element)){
      any(names(env[[class]]) %in% element)
  } else {
      any(ls(env) %in% class)
  }
}




###############################
#        CLEANING
###############################
remove_white_space <- function(orig_dat) {

  orig_dat[orig_dat==""] <- NA
  orig_dat
}

remove_trailing_spaces <- function(text_vec){
  gsub("^\\s+|\\s+$", "", text_vec)
}

remove_NA_cols <- function(orig_dat) {
  is_na_cols <- apply(orig_dat, 2, function(x) {
                        sum(is.na(x)) == length(x)
                      })

  if (!any(is_na_cols)) {
    orig_dat
  } else {
    warning("The following variables contained all NAs, and were dropped:\n",
            paste(names(orig_dat[is_na_cols]), collapse=",\n"))
    orig_dat[!is_na_cols]
  }
}





get_class <- function(new_dat) {
  new_dat %>%
    sapply(class) %>%
    sapply("[[", 1)
}




data_clean_up <- function(dat) {

  tracking_env <- attr(dat, "tracking_env")
  # browser()
  non_tidys <- sapply(dat, is, "non_tidy")

  if (!any(non_tidys)) {
    # subsetting out text vars
    return (assign_env(dat[non_text_indices(dat)], tracking_env))
  } else {

    # extract data for attributes before subset out non_text_data below
    the_non_tidy_col <- colnames(dat[non_tidys])
    key <- attr(dat[[the_non_tidy_col]], "key")

    # subsetting out text vars
    dat2 <- dat[non_text_indices(dat)]


    tracking_env[["class"]] <-  c("non_tidy")
    tracking_env[["key_var_data"]] <- list("key" = key
                                      , "the_non_tidy_col" = the_non_tidy_col
                                      , "data" = dat[[the_non_tidy_col]]) # key var, non_tidy var, non_tidy data
    assign_env(dat2, tracking_env)

  }
}








##########################################
#        Handles different types of Columns
#
#  1. One Answer Cols: For one project, the column value never changes
#  2. Redundant Cols : Several one answer columns, e.g. project name, project start date, project owner ....
#  3. Project ID: in a data set containing multiple projects,
#                  when you need to clean up an untidy variable, (data gather long too many times)
#                  Project ID, identifies where the project starts and ends, so that you can spread separately
##########################################



numerize <- function(x) {
    y <- as.character(as.factor(x))
    y[is.na(y)] <- "NA"
    as.numeric(as.factor(y))
}

# helper for redundant_cols
rle_comparison <- function(x, y){
    identical(rle(numerize(x))[[1]], rle(numerize(y))[[1]])
}



redundant_cols <- function(dat){

  . <- NULL

  tracking_env <- attr(dat, "tracking_env")

  ncols <- ncol(dat)
  cmat <- matrix(rep(NA, ncols^2), ncols)
  for (i in 1:ncols) {
    for (j in 1:ncols) {
      cmat[i, j] <- rle_comparison(.subset2(dat, i), .subset2(dat, j))
    }
  }
  dimnames(cmat) <- list(colnames(dat), colnames(dat))


  rle_candidates <- unique(lapply(rownames(cmat), function(rname) { colnames(cmat)[cmat[rname, ]==1] }))
  rle_candidates <- rle_candidates[sapply(rle_candidates, function(x) length(x) > 1)]
  # loop thru correlation matrix looking for 1.000
  # groups all variable names into sets of those with abs(correlation) == 1.00
  # and those that are not

  rle_candidates_class <- unlist(lapply(rle_candidates, function(x){
    sapply(dat[x], function(y)
    { class(y)[1]})
    }))

  if (length(rle_candidates) == 0) {
    return(assign_env(dat, tracking_env))
  } else {
    rle_unlist =unlist(rle_candidates)
    # browser()
    # rle_candidates is a list of vectors, ie,
    # vars to be grouped together because they will break chol potentially by being nonpositive definite
    lapply(rle_candidates, function(x) {
      # paste rows of rel_candidates together,
      # as.data.frame will do the coercing to factor
      do.call(paste, c(dat[x], sep="++++"))
      # paste_m <- factor(paste_m, levels=paste_m)
    } ) %>%
      as.data.frame(check.names = FALSE) %>%
      stats::setNames(.,unlist(lapply(rle_candidates, paste, collapse="++++"))) %>%
      dplyr::bind_cols(dat, .) %>%
      .[!colnames(.) %in% rle_unlist]  -> collapsed_data
      # list of group ids
      tracking_env[["class"]] <-  c("redundant_cols", tracking_env[["class"]])
      tracking_env[["redundant_cols"]] <- list("redundant_cols" = unlist(lapply(rle_candidates, paste, collapse="++++"))
                                                  # , "redundant_cols_data" = grpd[[unlist(lapply(rle_candidates, paste, collapse="++++"))]]
                                                  , "redundant_cols_classes" =  rle_candidates_class) # redundant_cols, and data

      assign_env(collapsed_data, tracking_env)

  }
}



# a function on whole dat, no lapply
one_answer_cols <- function(dat, type_of_one_answer) {

  tracking_env <- attr(dat, "tracking_env")

  var_lengths <- sapply(dat, function(x) length(unique(x)) ==1)
  if (sum(var_lengths)>0) {
    tracking_env[["class"]] <-  c(type_of_one_answer, tracking_env[["class"]])
    tracking_env[[type_of_one_answer]] <- setNames(list(dat[1, var_lengths, drop =FALSE]), type_of_one_answer) # redundant_cols, and data
      if(any(var_lengths)) {
        dat[var_lengths] <- NULL
      }
  }

  assign_env(dat, tracking_env)

}


project_id_finder <- function(data, key, untidy) {

  tracking_env <- attr(data, "tracking_env")
  #     What is definition of a key being nested inside of a project
  #
  #     Key must a set that repeats inside of its nest
  #     the nested column must have some id/category that repeats same length of key repeats (possibly with missingness)
  #
  key_to_non_tidy  <- tracking_env[["key_var_data"]][["key"]]
  non_tidy_colname <- tracking_env[["key_var_data"]][["the_non_tidy_col"]]
  #1. We know what key is
  #2. We can test for vars that repeat
  candidates <- names(sapply(data, function(x) length(unique(x))/nrow(data)<.9))
  candidates <- candidates[!candidates %in% c(key_to_non_tidy, non_tidy_colname)]
  # browser()
  #2.1 disqualify those candidates that have 1 level
  candidates <- candidates[sapply(candidates, function(x) length(unique(data[[x]]))!=1)]

  #3. using those as candidates,
  #3.0 find number of levels of each candidate, sort the data in order of fewest to most levels
  data <- dplyr::arrange_(data, .dots = lazyeval::interp(~names(sort(sapply(data[var], function(x) length(unique(x))))), var = as.name(candidates) ))

  # 3.2 disqualify those candidates in which key never repeats
  is_repeating <- function(candidate, key_var){
    all(sapply(split(key_var, candidate), function(x) length(x) != length(unique(x))))
  }
  candidates <- candidates[sapply(data[, candidates, drop = FALSE], is_repeating, data[[key_to_non_tidy]])]

  # 3.3 disqualify remaining candidates which have other still viable candidates that themselves can be nested and repeat inside of it
  project_id <- names(which.max(sapply(candidates, function(x) length(unique(data[[x]])))))
  project_id_classes <- sapply(data[project_id], function(x) class(x)[1])

  #     3.4 this should leave only viable candidates for project id,
  #     3.4.1 if their are multiple, they should potentially be redundant, test for this using redundant_cols test
  if (length(project_id)>1) {
    tidyr::unite_(data, col=paste(project_id, collapse="++++"), project_id, sep="++++") ->  project_ids_grouped

    tracking_env[["class"]] <-c("project_id", tracking_env[["class"]])
    tracking_env[["project_id"]] <- list("project_id_cols" = paste(project_id, collapse="++++"),
                                                    "project_id_data" = unique(data[, project_id, drop=FALSE]),
                                                    "project_id_classes" = project_id_classes )

      assign_env(project_ids_grouped, tracking_env)

  } else {
    tracking_env[["class"]] <-c("project_id", tracking_env[["class"]])

    tracking_env[["project_id"]]  <- list("project_id" = project_id,
                                          "project_id_data" = unique(data[, project_id, drop=FALSE]),
                                          "project_id_classes" = project_id_classes)
    assign_env(data, tracking_env)
  }

} # end of project_finder









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


spread_dat_to_int <- function(data) {

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







###############################
#       BOOTSTRAPPERS
###############################


# called in sample_datter
boot_numeric <- function(x, n_samples) {
  sample(jitter(scale(x),0.05), n_samples, TRUE)
}

# called in sample_datter
boot_integer <- function(x, n_samples) {

  . <- factors <- NULL

  as.data.frame(as.matrix(table(x)), stringsAsFactors = FALSE) %>%
    dplyr::add_rownames() %>%
    stats::setNames(c("factors","proportions")) %>%
    dplyr::mutate(factors = factor(factors, levels = factors)) %>%
    {
      sample(.[["factors"]] , n_samples, TRUE, prob=.[["proportions"]])
      } %>%
    as.numeric() %>% scale()
}

# called in sampler
sample_datter <- function(num_dat, index, nrows) {

  new_dat <- as.data.frame(matrix(NA, ncol = ncol(num_dat), nrow=nrows))

  if (sum(index) >0 ) {
    new_dat[,index] <- apply(num_dat[,index, drop=FALSE], 2, boot_numeric, n_samples=nrows)
  }

  if (sum(!index) >0 ) {
    new_dat[,!index] <- apply(num_dat[,!index, drop=FALSE], 2, boot_integer, n_samples=nrows)
  }


  stats::setNames(new_dat, colnames(num_dat))
}


###############################
#       SAMPLER
###############################

# called in the_sampler
sampler <- function(new_dat, nrows) {

  num_dat <- sapply(new_dat, as.numeric)
  dbl_index <- sapply(new_dat, is.double)
# browser()
  sim_dat <- matrix(rep(colMeans(num_dat, na.rm = TRUE), nrows), ncol=ncol(num_dat), byrow=TRUE) +
    sapply(sample_datter(num_dat, dbl_index, nrows), as.numeric) %*%
    chol(corpcor::make.positive.definite(cov(num_dat, use = "pairwise.complete.obs")))

      message("This is comparison of column means bet/ simulated and original data.\n ONLY NON-OPEN-TEXT COlUMNS.")
      their_scipen <- options()$scipen
      on.exit(options(scipen = their_scipen))
      options(scipen = 2)
      compare_col_means <- round(t(rbind(colMeans(sim_dat, na.rm = TRUE), colMeans(num_dat, na.rm = TRUE)) ),3)
      colnames(compare_col_means) <- c("sim", "orig")
      print(compare_col_means)

  as.data.frame(sim_dat)
}


the_sampler <- function(spread_dat, nrows, index){

  spread_dat %>%
    spread_dat_to_int() %>%
    sampler(nrows) %>%
    ungrouper(spread_dat, index) %>%
    back_to_the_future(spread_dat)
}

###############################
#       BACK to the Future
###############################



# funciton to round numeric to int and limit range of integers
to_integer <- function(sim_dat_vec, new_dat_vec){

  round_sim_dat <- round(sim_dat_vec, 0)
  round_sim_dat[round_sim_dat < min(as.numeric(new_dat_vec))] <- min(as.numeric(new_dat_vec)) # limit min range to min(orig)
  round_sim_dat[round_sim_dat > max(as.numeric(new_dat_vec))] <- max(as.numeric(new_dat_vec)) # limit max range to max(orig)
  as.integer(round_sim_dat)
}


# back to integer
back_to_integer <- function(sim_dat, new_dat) {

  indices <- colnames(new_dat)[get_class(new_dat) %in% "integer"]

  if (length(indices)==0)  return(sim_dat)

  sim_dat[indices] <- lapply(indices, function(i) {

    to_integer(sim_dat[[i]], new_dat[[i]])
  })
  sim_dat
}



# back to factor
back_to_factor <- function(sim_dat, new_dat) {

  . <- NULL

  indices <- colnames(new_dat)[get_class(new_dat) %in% "factor"]
  indices <- colnames(sim_dat)[colnames(sim_dat) %in% indices]

  if (length(indices)==0)  return(sim_dat)


  lapply(indices, function(i) {
    to_integer(sim_dat[[i]], as.numeric(new_dat[[i]]))
    }) -> new_dat_int

  new_dat %>%
    .[indices] %>%
    lapply(levels) -> new_dat_labels

  new_dat %>%
    .[indices] %>%
    lapply(.,function(x) {
      y <- as.numeric(x)
      z <- unique(y)
      zz <- sort(z)
      }) -> new_dat_levels

  sim_dat[indices]  <- Map(function(x,y,z) {
    factor(x,levels=y, labels=z)
    },
    x=new_dat_int, y=new_dat_levels, z= new_dat_labels)

    sim_dat
}

back_to_date <- function(sim_dat, new_dat, ...) {

  . <- NULL

  indices <- colnames(new_dat)[get_class(new_dat) %in% "Date"]

  if (length(indices)==0)  return(sim_dat)

  sim_dat %>%
    .[indices] %>%
    lapply(as.Date, origin = "1970-01-01", ...) -> sim_dat[indices]

  sim_dat
}


back_to_POSIXct <- function(sim_dat, new_dat,...) {

  . <- NULL

  indices <- colnames(new_dat)[get_class(new_dat) %in% "POSIXct"]


  if (length(indices)==0)  return(sim_dat)

  sim_dat %>%
    .[indices] %>%
    lapply(as.POSIXct, origin="1969-12-31 19:00:00", ...) -> sim_dat[indices]

  sim_dat
}


back_to_logical <- function(sim_dat, new_dat) {
  indices <- colnames(new_dat)[get_class(new_dat) %in% "logical"]

  if (length(indices)==0)  return(sim_dat)


  sim_dat[indices] <- sapply(indices, function(i) {
    sim_dat[i] <- as.logical(to_integer(sim_dat[[i]], new_dat[[i]]) )
  })

  sim_dat

}


ungrouper <- function(dat, grp_spr_dat, index) {
  tracking_env <- attr(grp_spr_dat, "tracking_env")

  . <- NULL

    ## check if any of project_id, one_answer_cols_or redudant cols
    proj_id_exists <- is_sth_a(tracking_env, "class", "project_id")
    one_ans_cols_exists <- is_sth_a(tracking_env, "class", "one_answer_cols")
    redundant_exists <- is_sth_a(tracking_env, "class", "redundant_cols")

    if( !any(c(proj_id_exists, one_ans_cols_exists, redundant_exists)) ) {
        return(  assign_env(dat, tracking_env))
    } else {

    var_classes <- list();

     if (proj_id_exists){

      project_id_cols          <- tracking_env[["project_id"]][["project_id"]]
      project_id_cols_data     <- tracking_env[["project_id"]][["project_id_data"]]
      project_id_cols_classes  <- sapply(tracking_env[["project_id"]][["project_id_classes"]], function(x) class(x)[1] )

      dat[project_id_cols] <- project_id_cols_data[index,][rep(1, nrow(dat)), ]

      if (any(grep("\\+{4}", project_id_cols))){
        var_list = unlist(strsplit(project_id_cols, "\\+{4}"))
        tidyr::separate_(dat[project_id_cols] , col = project_id_cols, into=var_list, sep="\\+{4}") %>%
          dplyr::bind_cols(dat[!colnames(dat) %in% project_id_cols] , . ) -> dat

       } else {
        var_classes[[1]] <- list(project_id_cols, project_id_cols_classes)
       }
     }
    if (one_ans_cols_exists){
      one_answer_cols          <- colnames(tracking_env[["one_answer_cols"]][["one_answer_cols"]])
      one_answer_cols_data     <- tracking_env[["one_answer_cols"]][["one_answer_cols"]]
      one_answer_cols_classes  <- sapply(tracking_env[["one_answer_cols"]][["one_answer_cols"]], function(x) class(x)[1] )

      var_classes[[length(var_classes)+1]] <- list(one_answer_cols, one_answer_cols_classes)
      dat[one_answer_cols] <- one_answer_cols_data[rep(1, nrow(dat)), ]

    }

    if (redundant_exists){
      redundant_cols          <- tracking_env[["redundant_cols"]][["redundant_cols"]]
      # one_answer_cols_data     <- tracking_env[["one_answer_cols"]]
      redundant_cols_classes  <- tracking_env[["redundant_cols"]][["redundant_cols_classes"]]

      var_classes[[length(var_classes)+1]] <- list(redundant_cols, redundant_cols_classes)

      # dat[redundant_cols] <- one_answer_cols_data[rep(1, nrow(dat)), ]

      if(any(colnames(dat) %in% redundant_cols)){
        var_list <- unlist(strsplit(redundant_cols, "\\+{4}"))

        ## ensure factor levels
        dat[redundant_cols] <- back_to_factor(dat[redundant_cols], grp_spr_dat)

        for (i in seq_along(redundant_cols)){

          tidyr::separate_(dat[redundant_cols[i]] , col = redundant_cols, into=var_list, sep="\\+{4}") %>%
            dplyr::bind_cols(dat[!colnames(dat) %in% redundant_cols[i]] , . ) -> dat
        }

        }
      }

      if (length(var_classes)>0) {
            dat[unlist(lapply(var_classes, function(x) names(x[[2]])))]  <- unlist(lapply (var_classes,
                                                                                   function(z){ Map(function(x, y){

                                                        if(x=="POSIXct") {
                                                          as.POSIXct(dat[[y]])
                                                        } else if(x=="Date" ) {
                                                          as.Date(dat[[y]])
                                                        } else if(x=="integer" ) {
                                                          as.integer(dat[[y]])
                                                        } else {
                                                          as.character(dat[[y]] )
                                                        }
                                                      }, z[[2]], names(z[[2]]))
                        }), recursive = FALSE)
      }

  }

  assign_env(dat, tracking_env)
}



# sample the data and take it back to original  data type
back_to_the_future <- function(ungrouped_dat, spread_dat){


  ungrouped_dat %>%
    back_to_integer(spread_dat) %>%
    back_to_factor(spread_dat)  %>%
    back_to_date(spread_dat) %>%
    back_to_POSIXct(spread_dat) %>%
    back_to_logical(spread_dat) %>%
    expose_undercover_na() %>%
    unlist_df_list_if_length_one()
}

expose_undercover_na <- function(dat){

  dat[] <- lapply(dat, function(x){

      x[as.character(x) == "UNDERCOVER_NA"] <- NA
      if (is.factor(x) && any(levels(x) == "UNDERCOVER_NA")) x <- droplevels(x)
      x
  })

  dat
}

unlist_df_list_if_length_one <- function(dat) {
  if(length(dat)==1){
    dat[[1]]
  } else {
    dat
  }
}

