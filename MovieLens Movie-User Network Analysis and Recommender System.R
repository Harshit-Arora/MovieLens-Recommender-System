library(data.table)
library(igraph)
library(Matrix)
library(dplyr)

###################################################### Loading data
user <- read.table("u.user")
col.names<-c('user id','age','gender','occupation','zip code')
user <- read.table("u.user",sep="|",col.names = col.names)
col.names<- c('user_id','movie_id','rating','unix_timestamp')
rat <- read.table('u.data',sep="\t",col.names=col.names)
col.names<- c('movie_id', 'movie_title' ,'release_date','video_release_date', 'IMDb_URL', 'unknown', 'Action', 'Adventure',
              'Animation', 'Childrens', 'Comedy', 'Crime', 'Documentary', 'Drama', 'Fantasy',
              'Film-Noir', 'Horror', 'Musical', 'Mystery', 'Romance', 'Sci_Fi', 'Thriller', 'War', 'Western')
movie <- read.csv('u.item',sep="|",encoding = "latin-1",header=FALSE, col.names = col.names)


##################################################### Creating matrixes

# Join movie and users by ratings, create two affiliation matrixes first.
like <- rat[rat$rating==4|rat$rating==5,][,c(1,2)]
dislike <- rat[rat$rating==1|rat$rating==2,][,c(1,2)]
like <-subset(like,user_id %in% unique(dislike$user_id))
dislike <-subset(dislike,user_id %in% unique(like$user_id))


like_af <- spMatrix(nrow=length(unique(like$user_id)),
                    ncol=length(unique(like$movie_id)),
                    i = as.numeric(factor(like$user_id)),
                    j = as.numeric(factor(like$movie_id)),
                    x = rep(1, length(as.numeric(like$user_id))) )
row.names(like_af) <- levels(factor(like$user_id))
colnames(like_af) <- levels(factor(like$movie_id))
like_matrix <- as.matrix(tcrossprod(like_af))

dislike_af <- spMatrix(nrow=length(unique(dislike$user_id)),
                       ncol=length(unique(dislike$movie_id)),
                       i = as.numeric(factor(dislike$user_id)),
                       j = as.numeric(factor(dislike$movie_id)),
                       x = rep(1, length(as.numeric(dislike$user_id))) )
row.names(dislike_af) <- levels(factor(dislike$user_id))
colnames(dislike_af) <- levels(factor(dislike$movie_id))
dislike_matrix <- as.matrix(tcrossprod(dislike_af))


################################################## EDA
ecount(like_graph %s% dislike_graph)/ecount(dislike_graph)  #92.9%


################################################## Movie-based analysis
# 1. Genre-based regression
rat <- as.data.table(rat)
rat$rate_year <- as.POSIXct(as.numeric(as.character(rat$unix_timestamp)),origin="1970-01-01",tz="GMT")
rat$rate_year <- substr(as.character(rat$rate_year), 1, 4)
rat <- rat[, avg_rating := mean(rating),by=list(movie_id,rate_year)]
rat <- rat[, num_rating := .N, by = list(movie_id,rate_year)]
rat_avg <- unique(rat[,c(2,5,6,7)])
genre <- left_join(movie,rat_avg, by='movie_id')
genre$year <- substr(genre$release_date, 8, 11)
summary(lm(avg_rating ~ num_rating + Action + Adventure + Animation + Childrens + Comedy + Crime + Documentary + Drama + Fantasy +
             Film.Noir + Horror + Musical + Mystery + Romance + Sci_Fi + Thriller + War + Western + factor(year) +factor(rate_year), genre))


# 2. Core-periperal regression based on the movie network: Yaping Zhang
################################################## movie matrix
movie_af<-t(like_af)
like_movie_matrix <- as.matrix(tcrossprod(movie_af))
movie_af_d<-t(dislike_af)
dislike_movie_matrix <- as.matrix(tcrossprod(movie_af_d))

getNetStats=function(net)
{
  deg_in = degree(net, mode = "in")
  deg_out = degree(net, mode = "out")
  close= closeness(net, mode = "total")
  betw = betweenness(net)
  eigcent= eigen_centrality(net)$vector
  prank = page_rank(net)$vector# page_rank creates a list object with some other items in it, we just want the actual scores, which are contained in the object called "vector"
  id=V(net)$name
  stats= as.data.table(list(id = id, deg_in = deg_in, deg_out = deg_out, close = close, betw = betw, eigcent=eigcent,prank = prank))
  return(stats)
}


like_movie_graph <- graph_from_adjacency_matrix(like_movie_matrix, weighted= TRUE, mode="undirected")
stat_like_m = getNetStats(graph_from_adjacency_matrix(like_movie_matrix, mode="undirected"))

dislike_movie_graph <- graph_from_adjacency_matrix(dislike_movie_matrix, weighted= TRUE, mode="undirected")
stat_dislike_m = getNetStats(graph_from_adjacency_matrix(dislike_movie_matrix, mode="undirected"))

################################################# all movie matrix
col.names<- c('user_id','movie_id','rating','unix_timestamp')
rat <- read.table('u.data',sep="\t",col.names=col.names)
rat <- as.data.table(rat)
rat <- rat[, avg_rating := mean(rating),by=list(movie_id)]
rat <- rat[, num_rating := .N, by = list(movie_id)]
rat$rate_year <- as.POSIXct(as.numeric(as.character(rat$unix_timestamp)),origin="1970-01-01",tz="GMT")
rat$rate_year <- substr(as.character(rat$rate_year), 1, 4)


movie_af <- spMatrix(nrow=length(unique(rat$movie_id)),
                     ncol=length(unique(rat$user_id)),
                     i = as.numeric(factor(rat$movie_id)),
                     j = as.numeric(factor(rat$user_id)),
                     x = rep(1, length(as.numeric(rat$movie_id))) )
row.names(movie_af) <- levels(factor(rat$movie_id))
colnames(movie_af) <- levels(factor(rat$user_id))
movie_matrix <- as.matrix(tcrossprod(movie_af))

movie_graph <- graph_from_adjacency_matrix(movie_matrix, weighted= TRUE, mode="undirected")
stat_movie = getNetStats(movie_graph)
rat_unique <- unique(rat[,c(2,5)])
colnames(stat_movie)=c("movie_id","deg_in","deg_out","close","betw", "eigcent","prank")
stat_movie$movie_id<-as.integer(stat_movie$movie_id)
stat_movie_all = left_join(stat_movie,rat_unique, by='movie_id')
df= left_join(rat,stat_movie, by='movie_id')
df= left_join(df,movie,by='movie_id')
df$year <- substr(df$release_date, 8, 11)

### regression

cor(df$avg_rating,df$close) #-0.1927603
cor(df$avg_rating,df$betw) #-0.3436778
cor(df$avg_rating,df$deg_in) #0.4867308
cor(df$avg_rating,df$eigcent) #5411232
cor(df$avg_rating,df$prank) #0.5379965

summary(lm(rating ~ close+num_rating + Action + Adventure + Animation + Childrens + Comedy + Crime + Documentary + Drama + Fantasy +
             Film.Noir + Horror + Musical + Mystery + Romance + Sci_Fi + Thriller + War + Western + factor(year) +factor(rate_year), data = df)) #-4.224e+03
summary(lm(rating ~ betw+num_rating + Action + Adventure + Animation + Childrens + Comedy + Crime + Documentary + Drama + Fantasy +
             Film.Noir + Horror + Musical + Mystery + Romance + Sci_Fi + Thriller + War + Western + factor(year) +factor(rate_year), data = df)) #-3.014e-04
summary(lm(rating ~ deg_in+num_rating + Action + Adventure + Animation + Childrens + Comedy + Crime + Documentary + Drama + Fantasy +
             Film.Noir + Horror + Musical + Mystery + Romance + Sci_Fi + Thriller + War + Western + factor(year) +factor(rate_year), data = df)) #8.683e-04
summary(lm(rating ~ eigcent+num_rating + Action + Adventure + Animation + Childrens + Comedy + Crime + Documentary + Drama + Fantasy +
             Film.Noir + Horror + Musical + Mystery + Romance + Sci_Fi + Thriller + War + Western + factor(year) +factor(rate_year), data = df)) #1.6620837
summary(lm(rating ~ prank+num_rating + Action + Adventure + Animation + Childrens + Comedy + Crime + Documentary + Drama + Fantasy +
             Film.Noir + Horror + Musical + Mystery + Romance + Sci_Fi + Thriller + War + Western + factor(year) +factor(rate_year), data = df)) #5.869e+02

################################################# User-based analysis
###### 1. Centrality measures
getNetStats=function(net)
{
  deg_in = degree(net, mode = "in")
  deg_out = degree(net, mode = "out")
  close= closeness(net, mode = "total")
  betw = betweenness(net)
  prank = page_rank(net)$vector # page_rank creates a list object with some other items in it, we just want the actual scores, which are contained in the object called "vector"
  id=V(net)$name
  stats= as.data.table(list(id = id, deg_in = deg_in, deg_out = deg_out, close = close, betw = betw, prank = prank))
  return(stats)
}
like_graph <- graph_from_adjacency_matrix(like_matrix, weighted= TRUE, mode="undirected")
like_graph <- simplify(like_graph)
stat_like = getNetStats(simplify(graph_from_adjacency_matrix(like_matrix, mode="undirected")))

dislike_graph <- graph_from_adjacency_matrix(dislike_matrix, weighted= TRUE, mode="undirected")
dislike_graph <- simplify(dislike_graph)
stat_dislike = getNetStats(simplify(graph_from_adjacency_matrix(dislike_matrix, mode="undirected")))

# Correlation between centrality measures
cor(stat_like$close,stat_like$betw)  # 0.9082345
cor(stat_like$deg_in,stat_like$betw) # 0.8602506
cor(stat_like$prank,stat_like$betw) # 0.8674449
cor(stat_like$close,stat_like$deg_in) # 0.9892067
cor(stat_like$close,stat_like$prank) # 0.9903363
cor(stat_like$deg_in,stat_like$prank)  # 0.9998835

# Network level centrality
centr_clo(simplify(graph_from_adjacency_matrix(like_matrix, mode="undirected")))
centr_betw(simplify(graph_from_adjacency_matrix(dislike_matrix, mode="undirected")))
centr_degree(simplify(graph_from_adjacency_matrix(dislike_matrix, mode="undirected")))

###### 2. Demographic-based regression
colnames(rat)[1] <- colnames(user)[1]
df <- data.frame()
df <- left_join(rat, user, by='user.id')
df <- left_join(df, genre[,c(1,5:24, 28)],by='movie_id')
summary(lm(rating ~ age + gender + occupation + Action + Adventure + Animation + Childrens + Comedy + Crime + Documentary + Drama + Fantasy +
             Film.Noir + Horror + Musical + Mystery + Romance + Sci_Fi + Thriller + War + Western  + factor(year) + factor(rate_year), df))


###### 3. Similarity analysis
like_sim <- as.matrix(dist(like_matrix))
dislike_sim <- as.matrix(dist(dislike_matrix))
like_coord <- as.data.table(cmdscale(dist(like_matrix)),keep.rownames=TRUE)
dislike_coord <- as.data.table(cmdscale(dist(dislike_matrix)),keep.rownames=TRUE)
colnames(like_coord) <- c('user.id','like_coord_1','like_coord_2')
colnames(dislike_coord) <- c('user.id','dislike_coord_1','dislike_coord_2')
df$user.id <- as.character(df$user.id)
df <- left_join(df, like_coord, by='user.id')
df <- left_join(df,dislike_coord, by='user.id')
summary(lm(rating ~ like_coord_1+ like_coord_2+ dislike_coord_1+ dislike_coord_2+ age + gender + occupation + Action + Adventure + Animation + Childrens + Comedy + Crime + Documentary + Drama + Fantasy +
             Film.Noir + Horror + Musical + Mystery + Romance + Sci_Fi + Thriller + War + Western  + factor(year) + factor(rate_year), df))


################################################# Recommendation system
###### 1. Core-peripheral structure?
# Evidence 1
c <- eigen_centrality(like_graph)$vector
c_cal <- c
concentration <- c()
cp <- rep(0,length(c))
for(i in 1:length(c)){
  index <- which.max(c_cal)
  c_cal[index] <- -1
  cp[index] <- 1
  new_concentration <- cor(c, cp)
  concentration <- append(concentration, new_concentration)
}
plot(1:914, concentration)
max_scores <- list(max(concentration, na.rm=TRUE),which.max(concentration))

c <- eigen_centrality(dislike_graph)$vector
c_cal <- c
concentration <- c()
cp <- rep(0,length(c))
for(i in 1:length(c)){
  index <- which.max(c_cal)
  c_cal[index] <- -1
  cp[index] <- 1
  new_concentration <- cor(c, cp)
  concentration <- append(concentration, new_concentration)
}
plot(1:914, concentration)
max_scores <- list(max(concentration, na.rm=TRUE),which.max(concentration))

# Evidence 2: Closeness

like_closeness <- closeness(like_graph)
hist(like_closeness)

dislike_closeness <- closeness(dislike_graph)
hist(dislike_closeness)

# Evidence 3: component size
# like
components = clusters(like_graph)
max(components$csize)/vcount(like_graph)

# dislike
components_dislike = clusters(dislike_graph)
max(components_dislike$csize)/vcount(dislike_graph)

# Evidence 4: diameters
diameter(like_graph)
diameter(dislike_graph)

# Evidence 5: coreness table
table(coreness(like_graph))
table(coreness(dislike_graph))

# Conclusion: Not clear core-periphearal structure.


###### 2. Recommendation system: similarity based
# Divide the dataset into training and validation
jac_sim <- as.data.frame(rat[,c(1:3)])

library(reshape2)
jac_sim_matrix <- acast(jac_sim,user.id ~movie_id, value.var="rating")
jac_for_recommend <- as.data.frame(as.matrix(dist(jac_sim_matrix)))
top_3_neighbours <- t(apply(jac_for_recommend,1,function(x)names(jac_for_recommend)[sort(head(order(x,decreasing=TRUE),3))]))
top_3_neighbours <- as.data.frame(top_3_neighbours)
top_5_neighbours <- t(apply(jac_for_recommend,1,function(x)names(jac_for_recommend)[sort(head(order(x,decreasing=TRUE),5))]))
top_5_neighbours <- as.data.frame(top_5_neighbours)
recommend <- data.frame()
for(i in rownames(top_5_neighbours)){
  top_5_movies <- rat[rat$user.id==top_5_neighbours[i,'V1']|rat$user.id==top_5_neighbours[i,'V2']|rat$user.id==top_5_neighbours[i,'V3']]
  top_5_movies <- t(as.data.frame(unique(top_5_movies[top_5_movies[order(-top_5_movies$avg_rating, ), ]$movie_id %in%rat[rat$user.id==i,]$movie_id]$movie_id)[1:5]))
  rownames(top_5_movies)<-i
  recommend <- rbind(recommend,top_5_movies)
}
# Movies recommended for user no.1
print(recommend[1,])