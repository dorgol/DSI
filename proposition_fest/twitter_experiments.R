# experimenting with twitter API

# Summary
  # The Twitter API platform offers three tiers of search APIs
    # Standared - free but only goes back a week
    # Premium - "Free and paid, either the last 30 days of Tweets or access to Tweets from as early as 2006." ?
        # Create "developer account", needs phone number, application describing expected use
        # few hours delay for approval
    # Enterprise - always paid
  # https://developer.twitter.com/en/docs/tweets/search/overview
  
  #  To apply for a developer account, please click here and wait for approval.

# rtweet
  # limited date range? -- only goes back a week?
  # this is a feature of the standard search API? 
# streamR
  # 

# reference
# https://www.r-bloggers.com/scraping-twitter-data-to-visualize-trending-tweets-in-kuala-lumpur/
# more packages to load to follow the above notes
# library(tidytext)
# library(tidyverse)
# library(stringr)
# library(stopwords)

install.packages("rtweet") #this is what jared used too for his prop scraping
install.packages("streamR") #from nyu smapp lab: https://smappnyu.org/research/data-collection-and-analysis-tools/


library(rtweet) # for search_tweets()
library(streamR)

search_tweets("#ucdavis", n = 500) #spawsn, https://api.twitter.com/oauth/authenticate?oauth_token=... where you can authorize it using your account. Do you need a twitter account for this?

search_tweets("#DataScience", n = 500, type = "popular") 

# Terms to play with from hannah ---

termlist = c("#roysquad", "#bethecowboy", "#Jagjaguwar",  "#mitski",
             "roysquad", '"be the cowboy"', "Jagjaguwar", "mitski")
scrapelist = lapply(termlist, search_tweets, n = 500, type = "recent")

lapply(scrapelist, apply, 2, range)

filterStream( file.name="~/Documents/DSI/proposition_fest/tweets_rstats.json", track="rstats", tweets=10)

library(ROAuth)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "xxxxxyyyyyzzzzzz"
consumerSecret <- "xxxxxxyyyyyzzzzzzz111111222222"
my_oauth <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret, requestURL=requestURL,
                             accessURL=accessURL, authURL=authURL)
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))