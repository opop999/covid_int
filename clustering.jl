using DataFrames;
using Clustering;
using Plots;
using RDatasets;
using RData;
using Distances;
using Random;

# Using K-Means
iris = dataset("datasets", "iris");
features = collect(Matrix(iris[:, 1:4])');
k = 3;
result = kmeans(features, k);

plot_means = scatter(iris.PetalLength, iris.PetalWidth,
    marker_z = result.assignments,
    color = :blue, legend = false)

# Using K-Medoids
features = Matrix(iris[:, 1:4]);
distance_matrix = pairwise(Euclidean(), features, dims = 1);
medoids_result = kmedoids(distance_matrix, k);
plot_medoid = scatter(iris.PetalLength, iris.PetalWidth,
    marker_z = results.assignments,
    color = :blue, legend = false);

# International dataset Clustering

matrix_subset = load("clustering_matrix.rds");
distance_matrix = pairwise(Euclidean(), matrix_subset, dims = 1);

silhouettes()

medoids_result = kmedoids(distance_matrix, 3);


plot_medoid = scatter(iris.PetalLength, iris.PetalWidth,
    marker_z = medoids_result.assignments,
    color = :blue, legend = false);

# Keeping the RNG seed number consistent
Random.seed!(4167);
