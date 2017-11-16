library(jsonlite)
library(stringr)
library(tidyverse)
library(forcats)
library(igraph)

library(devtools)
install_github("plotflow", "trinker")

library(plotflow)



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
ecneph= ecneph %>%  mutate(time = ymd_hms(timestamp)) # lubridate convert

ecneph = ecneph %>% mutate(date_by_week = round_date(time, unit="week")) # organise by week


ecneph=ecneph %>% mutate(week= date_by_week -days(3)) # shift days by three



# Count the number of 'a's in each element of string

  ecneph$num_people_tagged= str_count(ecneph$text, "@")


  library(lubridate)
  

  
 
 
  
ecneph %>% count(user)

ecneph$user = as.factor(ecneph$user)


# group by time range
# prepare a data frame from a time range


ecneph3 = ecneph %>%filter(time >= "2017-08-27", time <= "2017-09-1") %>% mutate(reply_retweet_sum= as.numeric(retweets)+as.numeric(replies)) %>% group_by(user)%>% summarise(
  activity = n(),
  link = sum(links),
  multimedia=sum(media),
  reply_rt_sum = sum(reply_retweet_sum),
  retweet=sum(as.numeric(retweets)),
  reply=sum(as.numeric(replies)),
  tagged =sum(num_people_tagged),
  mean_retweet = mean(as.numeric(retweets)),
  mean_replies = mean(as.numeric(replies))) %>% mutate(retweet_reply_ratio = mean_retweet/mean_replies) %>% arrange(desc(activity))

# ggplot with bar frequency

ecneph %>%filter(time >= "2017-08-27", time <= "2017-09-1") %>% mutate(reply_retweet_sum= as.numeric(retweets)+as.numeric(replies)) %>% group_by(user)%>% summarise(
  activity = n(),
  link = sum(links),
  multimedia=sum(media),
  reply_rt_sum = sum(reply_retweet_sum),
  retweet=sum(as.numeric(retweets)),
  reply=sum(as.numeric(replies)),
  tagged =sum(num_people_tagged),
  mean_retweet = mean(as.numeric(retweets)),
  mean_replies = mean(as.numeric(replies))) %>% mutate(retweet_reply_ratio = mean_retweet/mean_replies) %>% arrange(desc(activity)) %>%  ggplot(aes(x = user, y = activity)) + 
  geom_bar(stat = "identity", color = "black", fill = "grey")+coord_flip() #%>% ggplot(aes(x=activity,colour=user)) + 
 # geom_bar() # %>% ggplot( aes(activity ,colour=user))+geom_bar() 

# ggplot with percent

ecneph %>%filter(time >= "2017-08-27", time <= "2017-09-1") %>% mutate(reply_retweet_sum= as.numeric(retweets)+as.numeric(replies)) %>% group_by(user)%>% summarise(
  activity = n(),
  link = sum(links),
  multimedia=sum(media),
  reply_rt_sum = sum(reply_retweet_sum),
  retweet=sum(as.numeric(retweets)),
  reply=sum(as.numeric(replies)),
  tagged =sum(num_people_tagged),
  mean_retweet = mean(as.numeric(retweets)),
  mean_replies = mean(as.numeric(replies))) %>% mutate(retweet_reply_ratio = mean_retweet/mean_replies) %>% mutate(percent=(activity/sum(activity))*100)  %>% mutate(user = fct_reorder(user, percent, .desc = FALSE))  %>% 
 ggplot(aes(x= user ,y = percent)) + 
  geom_bar(stat = "identity", color = "black", fill = "grey")+coord_flip()

# Activity filtered greater than 10
ecneph  %>% mutate(reply_retweet_sum= as.numeric(retweets)+as.numeric(replies)) %>% group_by(user)%>% summarise(
  activity = n(),
  link = sum(links),
  multimedia=sum(media),
  reply_rt_sum = sum(reply_retweet_sum),
  retweet=sum(as.numeric(retweets)),
  reply=sum(as.numeric(replies)),
  tagged =sum(num_people_tagged),
  mean_retweet = mean(as.numeric(retweets)),
  mean_replies = mean(as.numeric(replies))) %>%filter(activity>10) %>% mutate(retweet_reply_ratio = mean_retweet/mean_replies) %>% mutate(percent=(activity/sum(activity))*100)  %>% mutate(user = fct_reorder(user, percent, .desc = FALSE))  %>% 
  ggplot(aes(x= user ,y = percent)) + 
  geom_bar(stat = "identity", color = "steelblue" , fill = "steelblue")+coord_flip()


# stacked bar chart 


ecneph  %>% mutate(reply_retweet_sum= as.numeric(retweets)+as.numeric(replies)) %>% group_by(user)%>% summarise(
  activity = n(),
  link = sum(links),
  multimedia=sum(media),
  reply_rt_sum = sum(reply_retweet_sum),
  retweet=sum(as.numeric(retweets)),
  reply=sum(as.numeric(replies)),
  tagged =sum(num_people_tagged),
  mean_retweet = mean(as.numeric(retweets)),
  mean_replies = mean(as.numeric(replies))) %>%filter(activity>10) %>% mutate(retweet_reply_ratio = mean_retweet/mean_replies) %>% mutate(percent=(activity/sum(activity))*100)  %>% filter(activity>=5) %>% select(user,activity,retweet) %>% gather("variable","value",2:3) %>% mutate(user = fct_reorder(user, value, .desc = FALSE)) %>%  ggplot( aes(x = user, y = value,fill=variable)) +
  geom_bar(stat='identity')+coord_flip()






# https://stackoverflow.com/questions/5208679/order-bars-in-ggplot2-bar-graph
# https://stackoverflow.com/questions/3548199/how-to-barplot-frequencies-with-ggplot2
# https://stackoverflow.com/questions/22332911/plot-frequency-table-in-r
# https://stackoverflow.com/questions/3695497/show-instead-of-counts-in-charts-of-categorical-variables


# Number of cars in each cla



# geom bar

ecneph %>%filter(time >= "2017-08-27", time <= "2017-09-1") %>% ggplot(aes( x = fct_infreq(user)))+geom_bar()+coord_flip()+xlab("Users")
  # bar chart horizontal,used forcats
# increasing frequency in forcats ,used fct_rev to reverse

ecneph %>%filter(time >= "2017-08-27", time <= "2017-09-1") %>% ggplot(aes( x = fct_rev(fct_infreq(user))))+geom_bar()+coord_flip()+xlab("Users")

ecneph %>%filter(time >= "2017-08-27", time <= "2017-09-1") %>% ggplot(aes(x=user))+geom_bar()+coord_flip()+xlab("Users")


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


# networks from scrape data
library(tidyverse)
raw = ecneph %>% select(user,text)

for (i in 1:length(raw$text)) { #Extract the usernames from the tweets mentions = 
  mentions= unlist(str_extract_all(tolower(raw$text[i]),"@[a-z0-9]{2,15}"))}


l <- length(raw$text)
tweeter <- vector(mode = "character", length = l)
tweets = raw$text
tweets[[1]]


# Compile the frequencies of each screen name
tweeter.freq <- table(tweeter)

lst <- regmatches(tweets, gregexpr("(?<=@)\\w+", tweets, perl = TRUE)) # separate all mentions which follow @
lst_1= as.data.frame(do.call(rbind, lapply(lst, `length<-`, max(lengths(lst))))) # convert character vector into data frame
summary(lst_1)

lst_1$source=ecneph$user # add source of tweets


head(lst_1)

edges= lst_1 %>% gather(variable,target,V1:V9) %>% select(source,target) # make edges data frame just having souce and a new variable target formed by gathering

edges1 = na.omit(edges) # omit NA i.e no mentions

write.csv(edges1,"edge.csv")
str(edges1)
edges1$source = as.character(edges1$source) # convert factor into character

str(edges)

edges$source = as.character(edges$source)

# wrap_strings to tie nodes better
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


# layout.by attr functions better in some nodes



g <- igraph::graph.data.frame(edges1, directed=TRUE)
plot(g)

# graph is dirty
# we can clean by eliminating those above certain mean or median degree of connections


V(g)$degree <- degree(g, mode="all")
cut.off <- mean(V(g)$degree)
cut.off_1 = median(V(g)$degree) # median more useful in asymmetric network


sub <- induced_subgraph(g, which(V(g)$degree>cut.off))

sub_1 <- induced_subgraph(g, which(V(g)$degree>cut.off_1))


plot(sub) # much cleaner

plot(sub,layout=layout.by.attr(sub, wc=1)) # prevents text collision


plot(sub_1)

plot(sub_1,layout=layout.by.attr(sub, wc=1),vertex.label.distance=10) # pre,vents text collision # not coming out well in this case, go back change cut off


# adjacency matrix

adjacencyMatrix = as_adjacency_matrix(g)
adjacencyMatrix



# Calculate page rank of mentions

page.rank(g)

p = page.rank(g)

# plot page rank
# http://is-r.tumblr.com/post/38240018815/making-prettier-network-graphs-with-sna-and-igraph

pageRank <- page.rank(g)$vector



as.matrix(sort(pageRank))


plot(pageRank, evcent(g, directed = TRUE)$vector)

# Now, to make the prettiest graph we can:
png("Twitter_graph.png", h = 800, w = 800, type = "cairo-png")
par(mai = c(0, 0, 0, 0))
pageRankColor <- hsv(0, 1, (pageRank - min(pageRank)) /
                       (max(pageRank) - min(pageRank)))
pageRankScaler <- pageRank * 10 + 1/2

# load library sna for gplot of page rank
adjacencyMatrix <- table(adjacencyMatrix)
rownames(adjacencyMatrix)
colnames(adjacencyMatrix)


# https://matthewlincoln.net/2014/12/20/adjacency-matrix-plots-with-r-and-ggplot2.html

graph=g


# calculate statistics
V(graph)$comm <- membership(optimal.community(graph))
V(graph)$degree <- degree(graph)
V(graph)$closeness <- centralization.closeness(graph)$res
V(graph)$betweenness <- centralization.betweenness(graph)$res
V(graph)$eigen <- centralization.evcent(graph)$vector

V(graph)$eigen
V(graph)$degree
V(graph)$comm
V(graph)$closeness

node_list <- get.data.frame(graph, what = "vertices")

# Determine a community for each edge. If two nodes belong to the
# same community, label the edge with that community. If not,
# the edge community value is 'NA'
edge_list <- get.data.frame(graph, what = "edges") %>%
  inner_join(node_list %>% select(name, comm), by = c("from" = "name")) %>%
  inner_join(node_list %>% select(name, comm), by = c("to" = "name")) %>%
  mutate(group = ifelse(comm.x == comm.y, comm.x, NA) %>% factor())


# Create a character vector containing every node name
all_nodes <- sort(node_list$name)

# Adjust the 'to' and 'from' factor levels so they are equal
# to this complete list of node names
plot_data <- edge_list %>% mutate(
  to = factor(to, levels = all_nodes),
  from = factor(from, levels = all_nodes))

# Create the adjacency matrix plot
ggplot(plot_data, aes(x = from, y = to, fill = group)) +
  geom_raster() +
  theme_bw() +
  # Because we need the x and y axis to display every node,
  # not just the nodes that have connections to each other,
  # make sure that ggplot does not drop unused factor levels
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  theme(
    # Rotate the x-axis lables so they are legible
    axis.text.x = element_text(angle = 270, hjust = 0),
    # Force the plot into a square aspect ratio
    aspect.ratio = 1,
    # Hide the legend (optional)
    legend.position = "none")

name_order <- (node_list %>% arrange(comm))$name

# Reorder edge_list "from" and "to" factor levels based on
# this new name_order
plot_data <- edge_list %>% mutate(
  to = factor(to, levels = name_order),
  from = factor(from, levels = name_order))


# ggplot again ordered

ggplot(plot_data, aes(x = from, y = to, fill = group)) +
  geom_raster() +
  theme_bw() +
  # Because we need the x and y axis to display every node,
  # not just the nodes that have connections to each other,
  # make sure that ggplot does not drop unused factor levels
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  theme(
    # Rotate the x-axis lables so they are legible
    axis.text.x = element_text(angle = 270, hjust = 0),
    # Force the plot into a square aspect ratio
    aspect.ratio = 1,
    # Hide the legend (optional)
    legend.position = "none")







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