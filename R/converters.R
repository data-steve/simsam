
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


