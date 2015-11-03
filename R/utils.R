


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






