
source('functions.R')
train <- TRUE
tweets <- import_tweets(train = train)
valid_stems <- .get_valid_stems()

f <- if(train) {
  do_fit_model
} else {
  predict_new
}
walk(valid_stems, ~f(tweets = tweets, stem = .x))


