library(jsonlite)
library(stringr)
library(tidyverse)
library(forcats)
library(igraph)



# resources

# https://stackoverflow.com/questions/28335715/r-how-to-filter-subset-a-sequence-of-dates
# https://www.google.co.in/search?client=ubuntu&channel=fs&q=grepl+in+r+example&ie=utf-8&oe=utf-8&gfe_rd=cr&ei=b8emWcG-F8O1vATQzoSgBw
# https://sites.google.com/site/miningtwitter/questions/talking-about/given-topic
# https://stackoverflow.com/questions/25664007/reorder-bars-in-geom-bar-ggplot2
# https://rstudio-pubs-static.s3.amazonaws.com/7433_4537ea5073dc4162950abb715f513469.html

# https://stackoverflow.com/questions/42240820/sorting-bars-in-a-bar-chart-with-ggplot2

# https://stackoverflow.com/questions/19420155/subset-rows-according-to-a-range-of-time

# https://stackoverflow.com/questions/36875223/r-cut-function-with-posixct-dates-creating-specific-categories-based-on-time-o
# https://stackoverflow.com/questions/44272706/how-to-select-time-range-in-posixct-format-when-time-range-spans-two-dates?rq=1


# start


ecneph = fromJSON("/home/anupam/ecn.json")

unique(ecneph$user)

unique(ecneph$text)

summary(as.factor(ecneph$user))

sort(ecneph$retweets)

ecneph$media = ifelse(grepl("pic.twitter.com",ecneph$text)==TRUE,1,0)

ecneph$links = ifelse(grepl("https",ecneph$text)==TRUE,1,0)

summary(ecneph$media)
summary(as.factor(ecneph$media))





# count number of tweets
ecneph=ecneph %>% mutate(active_tweets= as.numeric(replies)+as.numeric(retweets))
ecneph= ecneph %>%  mutate(time = ymd_hms(timestamp))

ecneph = ecneph %>% mutate(date_by_week = round_date(time, unit="week"))


ecneph=ecneph %>% mutate(week= date_by_week -days(3))



# Count the number of 'a's in each element of string

  ecneph$num_people_tagged= str_count(ecneph$text, "@")


  library(lubridate)
  

  
 
 
  
ecneph %>% count(user)

ecneph$user = as.factor(ecneph$user)
# group by retweet
ecneph3 = ecneph %>%filter(time >= "2017-08-27", time <= "2017-09-1") %>% mutate(reply_retweet_sum= as.numeric(retweets)+as.numeric(replies)) %>% group_by(user)%>% summarise(
activity = n(),
link = sum(links),
multimedia=sum(media),
reply_rt_sum = sum(reply_retweet_sum),
retweet=sum(as.numeric(retweets)),
reply=sum(as.numeric(replies)),
tagged =sum(num_people_tagged),
mean_retweet = mean(as.numeric(retweets)),
mean_replies = mean(as.numeric(replies))) %>% mutate(retweet_reply_ratio = mean_retweet/mean_replies) %>% arrange(desc(activity)) %>% ggplot( aes(activity ,colour=user))+geom_bar() 
# Number of cars in each cla
g + geom_bar()


# geom bar

ecneph %>%filter(time >= "2017-08-27", time <= "2017-09-1") %>% ggplot(aes( x = fct_infreq(user)))+geom_bar()+coord_flip()+xlab("Users")
  # bar chart horizontal,used forcats


# Animate

library(magick)

background <- image_read("/home/anupam/abc.png")
# And bring in a logo
image_info(background)
logo_raw <- image_read("/home/anupam/bum.gif") 


frames <- lapply(logo_raw, function(frame) {
  image_composite(background, frame, offset = "+250+420") # remeber +70+320 for travolta with normal saved ggplot
})

# more you increase second numberu in offset (+x+y) , gif image goes down towards x axis., more you increase x more to right

animation <- image_animate(image_join(frames))


image_write(animation, "/home/anupam/bumpeez.gif")


# 
#date >= "2014-12-02", date <= "2014-12-05"
# sseing if we can filter or note

ecneph %>% filter(time >= "2017-05-01", time <= "2017-05-06")

ecneph %>% mutate(daymonth =dm(time))

ecneph %>% mutate(reply_retweet_sum= as.numeric(retweets)+as.numeric(replies)) %>% group_by(user,date_by_week)%>% summarise(
  activity = n(),
  link = sum(links),
  multimedia=sum(media),
  reply_rt_sum = sum(reply_retweet_sum),
  retweet=sum(as.numeric(retweets)),
  reply=sum(as.numeric(replies)),
  tagged =sum(num_people_tagged),
  mean_retweet = mean(as.numeric(retweets)),
  mean_replies = mean(as.numeric(replies))) %>% mutate(retweet_reply_ratio = mean_retweet/mean_replies) %>% arrange(desc(activity)) %>% filter(user=="arvindcanchi") %>% 
  ggplot( aes(date_by_week, activity)) + geom_line() 


library(lubridate)
ecneph2 = ecneph %>% mutate(reply_retweet_sum= as.numeric(retweets)+as.numeric(replies)) %>% group_by(week,user)%>% summarise(
  activity = n(),
  link = sum(links),
  multimedia=sum(media),
  reply_rt_sum = sum(reply_retweet_sum),
  retweet=sum(as.numeric(retweets)),
  reply=sum(as.numeric(replies)),
  tagged =sum(num_people_tagged),
  mean_retweet = mean(as.numeric(retweets)),
  mean_replies = mean(as.numeric(replies))) %>% mutate(retweet_reply_ratio = mean_retweet/mean_replies)  %>% filter(user=="arvindcanchi") %>% 
  ggplot( aes(as.Date(week), activity) )+ geom_line() + scale_x_date(date_breaks = "1 week") + theme(axis.text.x = element_text(angle = 90, hjust = 1))+xlab("Week")







summary(count(as.factor(ecneph$date_by_week)))
  str(ecneph$date_by_week)
#https://blog.exploratory.io/5-most-practically-useful-operations-when-working-with-date-and-time-in-r-9f9eb8a17465

start_time = "2017-05-01 05:00:00"


summary(ecneph$date_by_week)
dplyr::filter(ecneph, grepl("https",text))

dplyr::filter(ecneph, grepl("pic.twitter.com",text))

ecneph$text[2]

summary(ecneph$timestamp)

format.str <- "%a %b %d %H:%M:%S %z %Y"

library(lubridate)

ecneph %>%  mutate(time = ymd_hms(timestamp))

save.image(file="ecn.RData")






# https://stackoverflow.com/questions/31092222/extracting-users-from-twitter-status-in-r-non-trivial-cases-like-rtuser-user

raw = ecneph %>% select(user,text)

for (i in 1:length(raw$text)) { #Extract the usernames from the tweets mentions = 
  mentions= unlist(str_extract_all(tolower(raw$text[i]),"@[a-z0-9]{2,15}"))}


l <- length(raw$text)
tweeter <- vector(mode = "character", length = l)
tweets = raw$text
tweets[[1]]


# Compile the frequencies of each screen name
tweeter.freq <- table(tweeter)

lst <- regmatches(tweets, gregexpr("(?<=@)\\w+", tweets, perl = TRUE))
lst_1= as.data.frame(do.call(rbind, lapply(lst, `length<-`, max(lengths(lst)))))
summary(lst_1)

lst_1$source=ecneph$user


head(lst_1)

edges= lst_1 %>% gather(variable,target,V1:V9) %>% select(source,target)

edges1 = na.omit(edges)


str(edges1)
edges1$source = as.character(edges1$source)

str(edges)

edges$source = as.character(edges$source)

wrap_strings <- function(vector_of_strings,width){
  as.character(sapply(vector_of_strings, FUN=function(x){
    paste(strwrap(x, width=width), collapse="\n")
  }))
}

# Function to increase node separation (for explanatory details, see the link below)
# Source: http://stackoverflow.com/a/28722680/496488
layout.by.attr <- function(graph, wc, cluster.strength=1,layout=layout.auto) {  
  g <- graph.edgelist(get.edgelist(graph)) # create a lightweight copy of graph w/o the attributes.
  E(g)$weight <- 1
  
  attr <- cbind(id=1:vcount(g), val=wc)
  g <- g + vertices(unique(attr[,2])) + igraph::edges(unlist(t(attr)), weight=cluster.strength)
  
  l <- layout(g, weights=E(g)$weight)[1:vcount(graph),]
  return(l)
}






g <- igraph::graph.data.frame(edges1, directed=TRUE)
plot(g)



V(g)$degree <- degree(g, mode="all")
cut.off <- mean(V(g)$degree)
cut.off_1 = median(V(g)$degree) # median more useful in asymmetric network


sub <- induced_subgraph(g, which(V(g)$degree>cut.off))

sub_1 <- induced_subgraph(g, which(V(g)$degree>cut.off_1))


plot(sub)

plot(sub,layout=layout.by.attr(sub, wc=1)) # prevents text collision


plot(sub_1)

plot(sub_1,layout=layout.by.attr(sub, wc=1),vertex.label.distance=10) # pre,vents text collision



E(sub)$width <- 1+E(sub)$weight/12
plot(sub_1,vertex.label.distance=5.0)


cut.off

V(sub)$value
plot(sub, vertex.shape="none", vertex.size=1,     
     
     layout=layout.by.attr(sub, wc=1))
str(lst_1)
mentions= unlist(lst)
sort(summary(as.factor(mentions)),decreasing = TRUE)
#https://sites.google.com/site/miningtwitter/questions/talking-about/given-topic

tryTolower = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}

results = sapply(mentions, tryTolower)
names(results) = NULL

results = results[results != ""]
library(tm)

# create corpus
corpus = Corpus(VectorSource(results))

# remove stopwords
skipwords = c(stopwords("english"), 
              "genetics", "genomics", "genetic", "genome")
corpus = tm_map(corpus, removeWords, skipwords)

# term-document matrix
tdm = TermDocumentMatrix(corpus)
# convert tdm to matrix
m = as.matrix(tdm)


# word counts
wc = rowSums(m)

# get those words above the 3rd quantile
lim = quantile(wc, probs=0.5)
good = m[wc > lim,]

# remove columns (docs) with zeroes
good = good[,colSums(good)!=0]
m

# adjacency matrix
M = good %*% t(good)
M

# set zeroes in diagonal
diag(M) = 0

library(igraph)
# graph
g = graph.adjacency(M, weighted=TRUE, mode="undirected",
                    add.rownames=TRUE)
# layout
glay = layout.fruchterman.reingold(g)
g
plot(g)
# let's superimpose a cluster structure with k-means clustering
# http://kateto.net/networks-r-igraph

# http://curleylab.psych.columbia.edu/netviz/netviz1.html#/52




kmg = kmeans(M, centers=8)
gk = kmg$cluster


library(graphTweets)

edges <- getEdges(data = raw, tweets = "text", source = "user")
nodes <- getNodes(edges)

# plot
g <- igraph::graph.data.frame(edges, directed=TRUE, vertices = nodes)

plot(g)


tail(ecneph,n=40)