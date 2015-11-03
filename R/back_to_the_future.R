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
    expose_undercover_na()
}

expose_undercover_na <- function(dat){

  dat[] <- lapply(dat, function(x){

      x[as.character(x) == "UNDERCOVER_NA"] <- NA
      if (is.factor(x) && any(levels(x) == "UNDERCOVER_NA")) x <- droplevels(x)
      x
  })

  dat
}

