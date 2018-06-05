#clustering with movies data<Hclust>
## Find which cluster Men in Black is in.

#Clustering with flowers dataset<Hclust>
### Select 3 clusters <Choosing clusters>

# Let's try this with an MRI image of the brain<KMM clust>
##The clustering is done for a healthy man

# Apply to a test image
##Clusters found in the above example will be applied to a test image<person show has tumors in brain>


#Download data from http://files.grouplens.org/datasets/movielens/ml-100k/u.item
#Load data into R
url='http://files.grouplens.org/datasets/movielens/ml-100k/u.item'
movies = read.table(url, header=FALSE, sep="|",quote="\"")

str(movies)

# Add column names
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary",
                     "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")

str(movies)

# Remove unnecessary variables
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL

# Remove duplicates
movies = unique(movies)

# Take a look at our data again:
str(movies)
movies <-movies[complete.cases(movies),]

# Compute distances
distances = dist(movies[2:20], method = "euclidean")

# Hierarchical clustering
clusterMovies = hclust(distances, method = "ward.D") 

# Plot the dendrogram
plot(clusterMovies)

# Assign points to clusters
rect.hclust(clusterMovies, k = 3, border = "red")
MovieClusters = cutree(clusterMovies, k = 3)
table(MovieClusters)

#Now let's figure out what the clusters are like.
#Let's use the tapply function to compute the percentage of movies in each genre and cluster

tapply(movies$Action, MovieClusters, mean)
tapply(movies$Romance, MovieClusters, mean)
tapply(movies$Adventure, MovieClusters, mean)
tapply(movies$Animation, MovieClusters, mean)
tapply(movies$Childrens, MovieClusters, mean)
tapply(movies$Comedy, MovieClusters, mean)

# We can repeat this for all the genre. If you do, you get the results in ClusterMeans.ods
for (m_type in 2:20){
  print(tapply(movies[,m_type], MovieClusters, mean)  )
}


# Find which cluster Men in Black is in.
subset(movies, Title=="Men in Black (1997)")
MovieClusters[257]


# Create a new data set with just the movies from cluster 2
cluster2 = subset(movies, MovieClusters==2)

# Look at the first 10 titles in this cluster:
cluster2$Title[1:10]


#---------------------------------------------------------------------------------------

#Clustering with flowers dataset
library(readr)
flower <- read_csv("C:/Users/v m kishore/OneDrive/Data sets/flower.csv",  col_names = FALSE)
View(flower)

# Change the data type to matrix
flowerMatrix = as.matrix(flower)
str(flowerMatrix)

# Turn matrix into a vector
flowerVector = as.vector(flowerMatrix)
str(flowerVector)

# Compute distances
distance = dist(flowerVector, method = "euclidean")

# Hierarchical clustering
Hcluster_model = hclust(distance, method="ward.D")

# Plot the dendrogram
plot(Hcluster_model)

# Select 3 clusters
rect.hclust(Hcluster_model, k = 3, border = "red")
flowerCluster = cutree(Hcluster_model, k = 3)
flowerCluster

# Find mean intensity values
tapply(flowerVector, flowerCluster, mean)

# Plot the image and the clusters
dim(flowerCluster) = c(50,50)
image(flowerCluster, axes = FALSE)

# Original image
dim(flowerVector) =c(50,50)
image(flowerVector,axes=FALSE)

#image in grayscale 
image(flowerMatrix,axes=FALSE,col=grey(seq(0,1,length=256)))

#---------------------------------------------------------------------------

# Let's try this with an MRI image of the brain
library(readr)
healthy <- read_csv("C:/Users/v m kishore/OneDrive/Data sets/healthy.csv", 
                    col_names = FALSE)
healthyMatrix = as.matrix(healthy)
str(healthyMatrix)

# Plot image
image(healthyMatrix,axes=FALSE,col=grey(seq(0,1,length=256)))

# Hierarchial clustering
healthyVector = as.vector(healthyMatrix)
distance = dist(healthyVector, method = "euclidean")

# Specify number of clusters
k = 5

# Run k-means
set.seed(1)
KMC = kmeans(healthyVector, centers = k, iter.max = 1000)
str(KMC)

# Extract clusters
healthyClusters = KMC$cluster
KMC$centers[2]

# Plot the image with the clusters
dim(healthyClusters) = c(nrow(healthyMatrix), ncol(healthyMatrix))

image(healthyClusters, axes = FALSE, col=rainbow(k))

#------------------------------------------------------
require(cluster)
X <- healthyMatrix
kmm <- kmeans(X, 5)
D <- daisy(X)
plot(silhouette(kmm$cluster, D), col=1:5,border=NA)
#------------------------------------------------------
# Apply to a test image
library(readr)
tumor <- read_csv("C:/Users/v m kishore/OneDrive/Data sets/tumor.csv", 
                  col_names = FALSE)
tumorMatrix = as.matrix(tumor)
tumorVector = as.vector(tumorMatrix)

# Apply clusters from before to new image, using the flexclust package
install.packages("flexclust")
library(flexclust)

KMC.kcca = as.kcca(KMC, healthyVector)
tumorClusters = predict(KMC.kcca, newdata = tumorVector)

# Visualize the clusters
dim(tumorClusters) = c(nrow(tumorMatrix), ncol(tumorMatrix))
image(tumorClusters, axes = FALSE, col=rainbow(k))
