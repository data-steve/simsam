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

  sim_dat <- matrix(rep(colMeans(num_dat, na.rm = TRUE), nrows), ncol=ncol(num_dat), byrow=TRUE) +
    sapply(sample_datter(num_dat, dbl_index, nrows), as.numeric) %*%
    chol(corpcor::make.positive.definite(cov(num_dat, use = "pairwise.complete.obs")))


  as.data.frame(sim_dat)
}


the_sampler <- function(grp_spr_dat, nrows, index){

  grp_spr_dat %>%
    grp_sprd_to_int() %>%
    sampler(nrows) %>%
    ungrouper(grp_spr_dat, index) %>%
    back_to_the_future(grp_spr_dat)
}


