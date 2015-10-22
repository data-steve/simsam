

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
