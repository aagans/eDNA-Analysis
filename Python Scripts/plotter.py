import seaborn as sns
import numpy as np
from scipy import stats
import matplotlib.pyplot as plt
from sklearn_extra.cluster import KMedoids
from sklearn.metrics import silhouette_samples, silhouette_score
import matplotlib.cm as cm


def plot_heatmap(normalized_count_df):
    count_hm = sns.heatmap(normalized_count_df)
    plt.show()
    return count_hm


def plot_linear_graph(sample_sums):
    x = sample_sums["sample number"]
    y = sample_sums["sums"]
    slope, intercept, r_value, p_value, std_err = stats.linregress(x, y)
    sample_linear = sns.regplot(data=sample_sums, x="sample number", y="sums")
    plt.text(0,0, f'R squared = {r_value**2}', horizontalalignment='left', size='small', color='black')
    return sample_linear


def plot_clustering_graphs(data, clustering_object, max_num_clusters, num_iterations):
    for num_clusters in range(2, max_num_clusters + 1):
        fig, (ax1, ax2) = plt.subplots(1, 2)
        fig.set_size_inches(18, 7)
        ax1.set_xlim([-0.1, 1])
        ax1.set_ylim([0, len(clustering_object[f"{num_clusters} clusters"]) + (num_clusters + 1) * 10])
        cluster_labels = clustering_object[f"{num_clusters} clusters"]

        silhouette_avg = silhouette_score(data, cluster_labels)
        print(
            "For n_clusters =",
            num_clusters,
            "The average silhouette_score is :",
            silhouette_avg,
        )
        sample_silhouette_values = silhouette_samples(data, cluster_labels)
        y_lower = 10
        for i in range(num_clusters):
            ith_cluster_silhouette_values = sample_silhouette_values[cluster_labels == i]

            ith_cluster_silhouette_values.sort()

            size_cluster_i = ith_cluster_silhouette_values.shape[0]
            y_upper = y_lower + size_cluster_i

            color = cm.nipy_spectral(float(i) / num_clusters)
            ax1.fill_betweenx(
                np.arange(y_lower, y_upper),
                0,
                ith_cluster_silhouette_values,
                facecolor=color,
                edgecolor=color,
                alpha=0.7,
            )
            ax1.text(-0.05, y_lower + 0.5 * size_cluster_i, str(i))
            y_lower = y_upper + 10

        ax1.set_title("The silhouette plot for the various clusters.")
        ax1.set_xlabel("The silhouette coefficient values")
        ax1.set_ylabel("Cluster label")

        ax1.axvline(x=silhouette_avg, color="red", linestyle="--")
        ax1.set_yticks([])
        ax1.set_xticks([-0.1, 0, 0.2, 0.4, 0.6, 0.8, 1])

        plt.suptitle(
            "Silhouette analysis for KMeans clustering on sample data with n_clusters = %d"
            % num_clusters,
            fontsize=14,
            fontweight="bold",
        )
        plt.show()


