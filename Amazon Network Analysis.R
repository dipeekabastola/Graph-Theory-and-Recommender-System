# Skip first line to avoid % bipartite unweighted" 
library(data.table)
library(igraph)
library(anytime)
library(splitstackshape)
library(MASS)
library(ggplot2)
library(ggthemes)
library(recommenderlab)
library(lubridate)
library(tidyr)
library(dplyr)


# Reading file and assigning column names 
dat <- fread('rec-amazon-ratings.edges')
colnames(dat) <- c('user','product','rating','date')
setorderv(dat, c('user','date'))

dat$date <- anytime(dat$date)

head(dat)

length(unique(dat$product))
  

 
# Extract year from date column 
dat$year <- format(dat$date, '%Y')
dat$year  <- as.numeric(dat$year)
head(dat)
  

 
min(dat$year)
max(dat$year)  
  

## Basic Summary
 
# number of unique movies
cat("Number of unique products:", length(unique(dat$product)))

# number of users
cat("\nNumber of users who provided ratings:",length(unique(dat$user)))

# number of total ratings
cat("\nNumber of total ratings:",nrow(dat))

# Years for rating 
cat("\nFirst year of rating:",min(dat$year))

cat("\nLast year of rating:",min(dat$year))
min(dat$year)
max(dat$year)  
  

 
nine <- dat[year==1997]
length(unique(nine$product))
length(dat[year==2006])

head(nine)

  


 
# Average and median user rating
cat("Average rating across data:",mean(dat$rating))
cat("\nMedian rating across data:",median(dat$rating))
  

# Rating Distribution
 
theme_update(plot.title = element_text(hjust = 0.5))

# Amazon hex codes: blue: #146eb4, black: #000000, orange: #ff9900
ggplot(dat, aes(x=rating)) + 
  geom_histogram(binwidth=0.3, col="gray", fill="#146eb4") + 
  labs(x="Customer Rating of Products", y="Number of Customers") +
  ggtitle("Distribution of Customer Ratings") +
  theme_few()
  


# Distribution of ratings by user
 
# distribution of average rating by user (indicating lack of uniformity)
avg_ratings <- dat[, .(avg_rate=mean(rating)), by=user]
ggplot(avg_ratings, aes(x=avg_rate)) +
  geom_histogram(binwidth=0.1, col="gray", fill="#146eb4") + 
  labs(x="Average Customer Rating", y="Number of Customers") +
  ggtitle("Distribution of Average Customer Ratings") +
  theme_few()
  

# Distribution of number of products each user has rated
 
# Looking at number of products rated by each user 

# get table with number of products rated by each user
user_ratings <- dat[, .("num_rating"=.N), by=user]

# average number of products rated
cat("Average number of products rated:",mean(user_ratings$num_rating))

# median number of products rated
cat("\nMedian number of products rated:",median(user_ratings$num_rating))

# max number of products rated
cat("\nMax number of products rated:",max(user_ratings$num_rating))

  
# Plot
 
plot.ratings <- user_ratings[,.(num_rating = ifelse(num_rating>=100, 100, num_rating))]
ggplot(plot.ratings, aes(x=num_rating)) + 
  geom_histogram(binwidth=1, col="gray", fill="#146eb4") + 
  labs(x="Number of Ratings per Customer", y="Number of Customers", title="Distribution of Number of Ratings per Customer") + 
  theme_minimal() + scale_x_continuous(breaks=seq(10,100,10),
                                       labels=c("10","20","30","40","50","60","70","80","90", "100+")) + theme_few()
  


# Distribution by product
 
# look at number of ratings per product 

# get table with number of product ratings
products_ratings <- dat[, .("num_ratings"=.N), by=product]

# average number of products rated
cat("Average number of ratings per product:",mean(products_ratings$num_rating))

# median number of products rated
cat("\nMedian number of ratings per product:",median(products_ratings$num_rating))

# max number of products rated
cat("\nMax number of ratings per product:",max(products_ratings$num_rating))

  

# Plots
 
plot.ratings.prod <- products_ratings[,.(num_ratings = ifelse(num_ratings>=100, 100, num_ratings))]
ggplot(plot.ratings.prod, aes(x=num_ratings)) + 
  geom_histogram(binwidth=1, col="gray", fill="#146eb4") + 
  labs(x="Number of Ratings per Product", y="Number of Products", title="Distribution of Number of Ratings per Product") + 
  theme_minimal() + scale_x_continuous(breaks=seq(10,100,10),
                                       labels=c("10","20","30","40","50","60","70","80","90", "100+")) + theme_few()
  

# Data Cleaning - skip this part go to load data


# Edgelist with all nodes from 1996 to 2006 

 

# For entire dataset
Sys.time()

prod_level <- aggregate(user~product, dat, paste, collapse=",")

users <- t(cSplit(prod_level, "user", ","))

possible_pairs <- lapply(seq_len(ncol(users)), function(i) users[,i])

for(i in seq_along(possible_pairs)){
  possible_pairs[[i]] = possible_pairs[[i]][!is.na(possible_pairs[[i]])]
}

# ignore products with only one user using index
index <- sapply(seq_along(possible_pairs), function(i) length(possible_pairs[[i]]) > 2)
possible_pairs <- possible_pairs[index]

edges <- lapply(seq_along(possible_pairs), function(i) data.table(t(combn(possible_pairs[[i]][-1], 2)), product = possible_pairs[[i]][1]))

edges <- rbindlist(edges)

# Only keeping edges that are not duplicates
edges <- edges[V1!=V2]

Sys.time()
  

# Store all edgelists as igraph objects 
 
# Creating igraph object 
#graph_all <- graph.data.frame(edges)

# Writing igraph object
#write_graph(graph_all)
  

# Data Table append function

 
# Function for appending data tables
dt.append <- function(x1, x2) {
  obj <- deparse(substitute(x1)) # get the actual object name as a string
  assign(obj, value = data.table::rbindlist(list(x1, x2)), envir = .GlobalEnv)}

  


# Edges for each year 
 
edge_yearly <- data.table('V1'=0,'V2'=0, 'product'=0, 'year'=0 )

values <- c(min(dat$year)+1:max(dat$year))

# can use values or iterate for every year to make computations faster
for (j in values){
  data <- dat[dat[, year== j]]
  
  prod_level <- aggregate(user~product, data, paste, collapse=",")
  
  users <- t(cSplit(prod_level, "user", ","))
  
  possible_pairs <- lapply(seq_len(ncol(users)), function(i) users[,i])
  
  for(i in seq_along(possible_pairs)){
    possible_pairs[[i]] = possible_pairs[[i]][!is.na(possible_pairs[[i]])]
  }
  
  # ignore products with only one user using index
  index <- sapply(seq_along(possible_pairs), function(i) length(possible_pairs[[i]]) > 2)
  possible_pairs <- possible_pairs[index]
  
  edges_l <- lapply(seq_along(possible_pairs), function(i) data.table(t(combn(possible_pairs[[i]][-1], 2)), product = possible_pairs[[i]][1]))
  
  edges_l <- rbindlist(edges_l)
  
  edges_l <- edges_l[V1!=V2]
  
  edges_l$year <- j
  
  edge_yearly <- dt.append(edge_yearly, edges_l)
}

egde_yearly <- edge_yearly[-1]

unique(edge_yearly$year)
  

 
# Creating igraph object 
# graph_year <- graph.data.frame(edge_yearly)

# Writing igraph object
# write_graph(graph_year)
  


# Save R workspace

 
save(edges, file = "edges.RData")
save(edge_yearly, file = "edgesyearly.RData")
  

 
load("edges.RData")
load("edgesyearly.RData")
  

# Variables for 1st regression
## Number of products bought together per year - Y 
## Year 
## Cumulative products bought until prior year
## Length of the tie 

 

edges_noprod <- edge_yearly[,-c('product')]
edges_noprod <- edges_noprod[-1]

# Number of products bought together per year - Y 
edges_noprod[, num_prod:= .N, by = c('V1', 'V2', 'year')]
edges_noprod <- unique(edges_noprod)

# Number of products bought per year:
#max(edges_noprod$num_prod)
#min(edges_noprod$num_prod)
#median(edges_noprod$num_prod)

# Cumulative products bought until prior year
edges_noprod[, cum_prod := cumsum(num_prod), by= c('V1', 'V2')]

# Lagging cumulative products
edges_noprod[, cum_prod_lag := .(shift(cum_prod, 1L, fill = NA,
                                       type = "lag")),by = c('V1', 'V2')]


# Length of tie if we start at 0
edges_noprod[, length_tie := (year - min(.SD)), by = c('V1', 'V2'),
             .SDcols='year']

max(edges_noprod$length_tie) # 9 years is maximum length of tie 

# Length of tie if we start at 1
edges_noprod[, length_tie_1 := (year - min(.SD)+1), by = c('V1', 'V2'),
             .SDcols='year']

# Looking at the dataset
head(edges_noprod)
tail(edges_noprod)

max(edges_noprod$num_prod)

# Creating a year and weight variable
edges_noprod[, year_lag := .(shift(year, 1L, fill = NA,
                                   type = "lag")),by = c('V1', 'V2')]
edges_noprod[, diff_years := 10 - (year - year_lag)]
  

 
#Save data file in workspace
 
save(edges_noprod, file = "edges_noprod.RData")
load("edges_noprod.RData")
  

#GLM Regression
 

# First regression analysis length tie is 0 - change if necessary
summary(glm.nb(num_prod ~ cum_prod_lag + length_tie + factor(year), edges_noprod))



# First regression analysis length tie is 1
summary(glm.nb(num_prod ~ cum_prod_lag + length_tie_1 + factor(year), edges_noprod))

# Do we need to offset for total ties that year/ how would we do it 
  


Variables for 2nd regression
- Difference in rating - Y  
- Year 
- Cumulative products bought until prior year
- Length of tie 

 

# Average rating for year
avg_rate <- dat[, -c('date','product')]

# Calculating average rating in
avg_rate <- avg_rate[, .(rating = mean(rating)), by = c('user','year') ]

#Viewing avg rating dataset
head(avg_rate)

# Setting keys before merging 
setkeyv(avg_rate, c('user', 'year'))
names(edges_noprod)[1] <- 'user'
setkeyv(edges_noprod, c('user','year'))

# Merging datasets to get rating info 
egdes_rating <- merge(edges_noprod, avg_rate, all=TRUE,
                      allow.cartesian =TRUE)


# Changing key to user 2 for the edges_rating dataset
names(egdes_rating)[1] <- 'user1'
names(egdes_rating)[9] <- 'user1rating'
names(egdes_rating)[3] <- 'user'


# Setting keys and deleting rows with no user 2 data
setkeyv(egdes_rating, c('user','year'))
egdes_rating <- egdes_rating[complete.cases(egdes_rating[,'user']),]

# Merge with user 2 rating
egdes_rating <- merge(egdes_rating, avg_rate, all=TRUE,
                      allow.cartesian =TRUE)

names(egdes_rating)[10] <- 'user2rating'
names(egdes_rating)[1] <- 'user2'

# Keeping rows with values in user 1 and 2
egdes_rating <- egdes_rating[complete.cases(egdes_rating[,c('user1',
                                                            'user2')]),]

# setting order
setcolorder(egdes_rating, c('user1','user2','year'))

# Difference in rating 
egdes_rating[, difference_rate:= abs(user1rating - user2rating)]

# Viewing data for merged dataset
head(egdes_rating)
head(egdes_rating)

  

Save data file in workspace
 
save(egdes_rating, file = "edges_rating.RData")
load("edges_rating.RData")
  



Graphing the user to product network
 

set.seed(123)
# Sample data
index <- sample(1:nrow(dat), 1000)
sample_data <- dat[index,]

# make bipartite graph
graph_bp <- graph.data.frame(sample_data[,1:2], directed=FALSE) # make general undirected graph

V(graph_bp)$type <- V(graph_bp)$name %in% sample_data$product # specify type to make bipartite
E(graph_bp)$weight <- sample_data$rating # add in rating as weight

# look at graph
graph_bp
  


  {r fig.width=20, fig.height=20}
# Visualize graph layout
# define color and shape mappings.
col <- c("#146eb4", "#000000")
shape <- c("circle", "circle")

plot(graph_bp,
     vertex.color = col[as.numeric(V(graph_bp)$type)+1],
     vertex.shape = shape[as.numeric(V(graph_bp)$type)+1], layout=layout_as_bipartite(graph_bp, hgap=100),
     vertex.frame.color="gray80",
     edge.color = "#ff9900",
     vertex.label="", vertex.size=5)
  

 

# make bipartite graph
graph_bp_full <- graph.data.frame(dat[,1:2], directed=FALSE) # make general undirected graph

V(graph_bp_full)$type <- V(graph_bp_full)$name %in% dat$product # specify type to make bipartite
E(graph_bp_full)$weight <- dat$rating # add in rating as weight

# look at graph
graph_bp_full

  



Calculate centrality measures - customer to product network
 
# calculate co-affiliation centrality measures
degree <- degree(graph_bp_full)
closeness <- closeness(graph_bp_full)
eigen <- eigen_centrality(graph_bp_full)

prod_rating <- dat[, .(avg_rating=mean(rating)), by=product]

prod_measures <- data.table("product"= V(graph_bp_full)[V(graph_bp_full)$type == TRUE], "degree"= degree[1:nrow(prod_rating)], "eigen_centrality"=eigen[1:nrow(prod_rating)])
#"closeness"=closeness[1:nrow(prod_rating)],

# Same type for both
prod_rating$product <- as.numeric(prod_rating$product)
prod_measures$product <- as.numeric(prod_measures$product)

# Setting keys and merging
prod_measures <- merge(prod_measures, prod_rating,
                       by='product')

  


Save file in workspace
 
save(prod_measures, file = "prod_measures.RData")
load("prod_measures.RData")
head(prod_measures)
  

Correlation between measures 
 
cor(prod_measures[,c(2,3)], use="pairwise.complete.obs")
  

 
hist(prod_measures$avg_rating)
hist(log(prod_measures$avg_rating))
hist(sqrt(prod_measures$avg_rating))
  


Using measures to predict ratings
 
summary(lm(avg_rating ~  log(degree) + log(eigen_centrality), prod_measures))
  

Using measures to predict ratings
 
summary(lm(avg_rating ~  log(degree), prod_measures))
  


Plot vs Avg rating 
 
# Plotting Degree Centrality
ggplot(prod_measures[degree<3000], aes(x=degree, y=avg_rating)) +
  geom_point(color = "#ff9900", fill = "#ff9900", alpha = 0.3) +   
  geom_smooth(method="loess", se=F, col="",) +
  labs(x="Product Degree Centrality", y="Average Product Rating") +
  ggtitle("Average Product Rating vs. Degree Centrality") +
  theme_few()
  

Plot vs Avg rating 
 
# Plotting Degree Centrality
ggplot(prod_measures[degree<3000], aes(x=degree, y=avg_rating)) +
  geom_point(color = "#ff9900", fill = "#ff9900", alpha = 0.3) +   
  geom_smooth(method="lm", se=F) +
  labs(x="Product Degree Centrality", y="Average Product Rating") +
  ggtitle("Average Product Rating vs. Degree Centrality") +
  theme_few()
  
 
# Plotting Degree Centrality
ggplot(prod_measures[degree<3000], aes(x=degree, y=avg_rating)) +
  geom_point(color = "#ff9900", fill = "#ff9900", alpha = 0.3) +   
  geom_smooth(method="loess", se=F) +
  labs(x="Product Degree Centrality", y="Average Product Rating") +
  ggtitle("Average Product Rating vs. Degree Centrality") +
  theme_few()
  

 
# Plotting eigen centrality with geom smooth
ggplot(prod_measures, aes(x=eigen_centrality, y=avg_rating)) +
  geom_smooth(method="lm", se=F, col="") +
  geom_point(fill = "#ff9900", alpha = 0.3) + 
  labs(x="Product Eigen Centrality", y="Average Product Rating") +
  ggtitle("Average Product Rating vs. Eigen Centrality") +
  theme_few()
  

 
# Plotting eigen centrality with geom smooth
ggplot(prod_measures, aes(x=eigen_centrality, y=avg_rating)) +
  geom_smooth(method="loess", se=F, col="") +
  geom_point(fill = "#ff9900", alpha = 0.3) + 
  labs(x="Product Eigen Centrality", y="Average Product Rating") +
  ggtitle("Average Product Rating vs. Eigen Centrality") +
  theme_few()
  


 
head(egdes_rating)
head(edge_yearly)
head(edges_noprod)
  

Edge decays 

 
decay <- edges_noprod

# Lagging year and calculating time since most recent purchase
decay[, year_lag := .(shift(year, 1L, fill = NA,
                            type = "lag")),by = c('V1', 'V2')]

decay[, tie_age := year - year_lag]

# replace nas with 0s
decay$tie_age <- coalesce(decay$tie_age, 0)

#subsetting decay 
decay <- decay[tie_age <= 3, ]

tail(decay)
  

Save file in workspace
 
save(decay, file = "decay.RData")
load("decay.RData")
  



Graphing decayed object 
 
# make graph object for decayed ties
graph_decay <- graph.data.frame(decay[,1:2], directed=FALSE) # make general undirected graph

E(graph_decay)$weight <- decay$num_prod  # add in number of products as weight

# look at graph
graph_decay
  

save graph object
 
save(graph_decay, file = "graph_decay.RData")
load("graph_decay.RData")
  



 
# Calculate centrality measures for user-user network
degree_dec <- degree(graph_decay)
#closeness_dec <- closeness(graph_decay)
eigen_dec <- eigen_centrality(graph_decay)$vector
#betweenness <- betweenness(graph_decay)

user_rating <- dat[, .(avg_rating=mean(rating)), by=user]

user_measures <- data.table("user"= V(graph_decay), "degree"= degree_dec, "eigen_centrality"=eigen_dec)
#, "closeness"=closeness_dec

# Same type for both
user_rating$user <- as.numeric(user_rating$user)
user_measures$user <- as.numeric(user_measures$user)

# Setting keys and merging
user_measures <- merge(user_measures, user_rating,
                       by='user')

tail(user_measures)
  

Save user measures
 
save(user_measures, file = "user_measures.RData")
load("user_measures.RData")
  


Transitivity 
 
# Calculating transitivity for ties without decay
transitivity(graph_decay)
  

Correlation between measures 
 
cor(user_measures[,c(2,3)], use="pairwise.complete.obs")
  

Variable Transformation
 
hist(user_measures$degree)
hist(log(user_measures$degree))
  

 
hist(user_measures$avg_rating)
hist(log(user_measures$avg_rating))
  


 
ggplot(user_measures, aes(x=eigen_centrality)) + 
  geom_histogram(binwidth=0.01, col="gray", fill="#146eb4") + 
  labs(x="Customer Eigen Centrality", y="Frequency") +
  ggtitle("Distribution of Customer Eigen Centrality") +
  theme_few()
  

 
ggplot(user_measures, aes(x=log(eigen_centrality))) + 
  geom_histogram(binwidth=0.3, col="gray", fill="#146eb4") + 
  labs(x="Log of Customer Eigen Centrality", y="Frequency") +
  ggtitle("Distribution of Log of Customer Eigen Centrality") +
  theme_few()
  



 
hist(user_measures$eigen_centrality)
hist(log(user_measures$eigen_centrality))
  


Using measures to predict ratings
 
summary(lm( avg_rating ~  log(degree) + log(eigen_centrality+.00001), user_measures))
  

Plot vs Avg rating 
 
# Plotting Degree Centrality
ggplot(user_measures, aes(x=degree, y=avg_rating)) +
  geom_point(color = "#ff9900", fill = "#ff9900", alpha = 0.3) +   
  geom_smooth(method="lm", se=F) +
  labs(x="User Degree Centrality", y="Average Rating") +
  ggtitle("Average Product Rating vs. Degree Centrality") +
  theme_few()
  


 
# Plotting Degree Centrality
ggplot(user_measures, aes(x=degree, y=avg_rating)) +
  geom_point(color = "#ff9900", fill = "#ff9900", alpha = 0.3) +   
  geom_smooth(method="loess", se=F) +
  labs(x="User Degree Centrality", y="Average Rating") +
  ggtitle("Average Product Rating vs. Degree Centrality") +
  theme_few()
  

 
# Plotting eigen centrality with geom smooth
ggplot(user_measures, aes(x=eigen_centrality, y=avg_rating)) +
  geom_smooth(method="lm", se=F, col="") +
  geom_point(color = "#ff9900", fill = "#ff9900", alpha = 0.3) + 
  labs(x="User Eigen Centrality", y="Average Rating") +
  ggtitle("Average Product Rating vs. Eigen Centrality") +
  theme_few()
  

 
# Plotting eigen centrality with geom smooth
ggplot(user_measures, aes(x=eigen_centrality, y=avg_rating)) +
  geom_smooth(method="loess", se=F, col="") +
  geom_point(color = "#ff9900", fill = "#ff9900", alpha = 0.3) + 
  labs(x="User Eigen Centrality", y="Average Rating") +
  ggtitle("Average Product Rating vs. Eigen Centrality") +
  theme_few()
  

