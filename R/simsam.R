#' Simulate Randomly Generated Correlated Data
#'
#' Simulate n rows of a randomly generated data set from an existing data set.
#' General structure (column means and correlations) are preserved.
#'
#' @param orig_dat A data set the user wishes to generate a random, correlated
#' data set for.
#' @param nrows The number of rows to make the output data set.
#' @param your_choice_of_NA A vector of characters that denote missing values
#' (e.g., "N/A", 999).
#' @return Returns a data set of randomly generated data corelated with the
#' riginal data set.
#' @export
#' @importFrom magrittr %>%
#' @examples
#' library(dplyr)
#' (sim_dat <- simsam(mtcars, 1000)[[1]] %>% tbl_df())
#' colMeans(sim_dat); colMeans(mtcars)
#' cor(sim_dat); cor(mtcars)
simsam <- function(orig_dat, nrows, your_choice_of_NA = NULL){

  if(!is.null(your_choice_of_NA)){
    dat[dat == your_choice_of_NA] <- NA
  }

  orig_dat %>%
    parse_dat() %>%

    # handling
    ## elimination on non-text data inside  AND
    ## giving key_var_data attributes to dataframe

    data_clean_up()  -> parsed_out

  parsed_out %>%
    one_answer_cols("one_answer_cols") %>%
    redundant_cols() -> redundant_cols_dat


  redundant_cols_dat %>%
    remaining_guid_emails_to_factor() %>%
    spread_untidy()  -> spread_dat


    spread_out <- list(rep(NA, length(spread_dat)))
	for (i in seq_along(spread_dat)) {
        spread_out[[i]] <- the_sampler(spread_dat[[i]], nrows, i)
    }
    spread_out
}




