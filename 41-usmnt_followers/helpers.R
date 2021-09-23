
# Reference: https://stackoverflow.com/questions/45152331/how-to-get-all-the-twitter-followers-ids-75000-of-a-user-using-pagination-in
# Global variables:
ids <- 75000 # Max number of ids per token every 15 minutes
res <- list() # Vector where user_ids from followers will be appended

# Function to get all the followers from a user with pagination
get_all_followers <- function(userId, followers, page) {
  
  if (ids == 0) {
    
    # API Twitter Limit reached - Wait
    message("Waiting 15 mins...")
    total <- 15*60 # Total time = 15 min ~ 900 sec
    pb <- txtProgressBar(min = 0, max = total, style = 3) # create progress bar
    
    for (i in 1:total) {
      Sys.sleep(time = 1) # 1 second interval
      setTxtProgressBar(pb, i) # update progress bar
    }
    close(pb)
    
    # Check rate limit followers/ids query
    if (!rate_limit(token = NULL)[38,]$reset > 14.9) {
      message("Waiting 15 seconds more...")
      Sys.sleep(time = 15) # wait 15 seconds more...
    }
    
    message("Go!")
    ids <<- 75000
  }
  
  if (followers <= ids) {
    
    message(paste("Followers < ids | Number of Followers: ",
                  followers, " | Number of resting ids: ",  ids, sep = ""))
    ftemp <- get_followers(user = userId, n = followers, page = page)
    
    if (page == '-1') {
      res <<- append(res, list(ftemp)) # append followers ids
    }
    
    if (page != '-1') {
      # df <- data.frame('user_id' = ftemp)
      
      res <<- append(res, list(df)) # append followers ids
    }
    
    ids <<- ids - followers
    message("Finished!")
    rtemp <- res
    res <<- list()
    return(rtemp)
    
  } else if (followers > ids) {
    
    message(paste("Followers > ids | Number of Followers: ",
                  followers, " | Number of resting ids: ",  ids, sep = ""))
    ftemp <- get_followers(user = userId, n = ids, page = page)
    
    if (page == '-1') {
      res <<- append(res, list(ftemp)) # append followers ids
    }
    
    if (page != '-1') {
      # browser()
      # df <- data.frame('user_id' = ftemp)
      res <<- append(res, list(ftemp)) # append followers ids
    }
    
    n <- ids # n = count of followers ids already acquired
    
    pageTemp <- next_cursor(ftemp) # Pagination
    print(pageTemp)
    # browser()
    # API Twitter Limit reached - Wait
    message("Waiting 15 mins...")
    total <- 15*60 # Total time = 15 min ~ 900 sec
    pb <- txtProgressBar(min = 0, max = total, style = 3) # create progress bar
    
    for (i in 1:total) {
      Sys.sleep(time = 1) # 1 second interval
      setTxtProgressBar(pb, i) # update progress bar
    }
    close(pb)
    
    # Check rate limit followers/ids query
    if (!rate_limit(token = NULL)[38,]$reset > 14.9) {
      message("Waiting 15 seconds more...")
      Sys.sleep(time = 15) # wait 15 seconds more...
    }
    
    message("Go!")
    ids <<- 75000
    
    # Recursive function call
    get_all_followers(userId = userId, followers = followers - n, page = pageTemp)
  }
}
