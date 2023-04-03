import math

import numpy as np
import pandas as pd
import scipy.spatial.distance as distance
from sklearn_extra.cluster import KMedoids
from plotter import plot_clustering_graphs, plot_heatmap


def select_file(filename):
    """Imports a CSV file of OTUs x samples as a pandas dataframe"""
    raw_count = pd.read_csv(filename, index_col=0)
    return raw_count


def delete_single_otu(row):
    """Function used in trimming to declare if an OTU only occurs in one sample"""
    no_zeros_series = row.where(row != 0).dropna()
    if len(no_zeros_series) == 1:
        return True
    else:
        return False


def remove_rare_units(count_df):
    """Trims the dataset by removing OTUs which only occur in one sample"""
    summation_df = count_df
    summation_count = summation_df.sum(axis=1)
    summation_df["sum"] = summation_count
    no_1_df = summation_df[summation_df['sum'] != 1]
    no_1_df = no_1_df.drop('sum', axis=1)
    trimmed_bool_series = no_1_df.apply(delete_single_otu, axis=1)
    summation_df["single sample"] = trimmed_bool_series
    trimmed_df = summation_df[summation_df['single sample'] == False]
    trimmed_df = trimmed_df.drop('single sample', axis=1)

    return trimmed_df


def normalize_count_df(count_df, normalizer):
    """Returns a normalized dataframe from a df of count data.
    Either proportional or log10 normalization is used"""
    if normalizer == 'proportional':
        sum_series = pd.Series(count_df.sum(axis=1))
        prop_count = count_df.iloc[:, 0:].div(sum_series, axis=0)
        return prop_count
    elif normalizer == 'log10':
        no_zero_count = count_df.replace(0, 0.00001)
        log_count = no_zero_count.applymap(math.log10)
        log_count.replace(-5, 0, inplace=True)
        return log_count


def sample_analysis(count_df):
    """Returns a dataframe of samples and the number of OTUs
    present in each sample, sorted in descending order"""
    sum_series = pd.DataFrame(count_df.sum(axis=0), columns=["sums"])
    sorted_samples_df = sum_series.sort_values(by="sums", axis=0, ascending=False)
    df_dimensions = sorted_samples_df.shape
    range_list = range(1, df_dimensions[0] + 1)
    sorted_samples_df["sample number"] = range_list
    return sorted_samples_df


def jsd_row(given_array, data):
    """Returns an array of Jensen Shannon Distance values comparing a given sample with all others"""
    num_rows, num_cols = np.shape(data)
    tiled_row = np.tile(given_array, [num_cols, 1])
    reversed_df = data.transpose()
    sample_array = reversed_df.to_numpy()
    jsd_value_array = distance.jensenshannon(tiled_row, sample_array, axis=1)
    return jsd_value_array


def jsd_matrix(count_df):
    """Returns a matrix of Jensen Shannon Distance values between samples"""
    distance_series = count_df.apply(jsd_row, axis=0, result_type='reduce', data=count_df).reset_index(drop=True)
    distance_matrix = pd.DataFrame(distance_series.values.tolist(), index=count_df.columns, columns=count_df.columns)
    return distance_matrix


def kmedoid_clustering(distance_matrix, max_num_clusters, num_iterations):
    """Clusters OTUs into clusters based on provided distance matrix"""
    kmedoids_dict = {}
    for num_clusters in range(2, max_num_clusters+1):
        kmedoids = KMedoids(n_clusters=num_clusters,
                            metric='precomputed',
                            max_iter=num_iterations).fit_predict(distance_matrix)
        kmedoids_dict[f'{num_clusters} clusters'] = kmedoids
    return kmedoids_dict


imported_df = select_file("raw_count_18S_ordered.csv")
trimmed_data_df = remove_rare_units(imported_df)
normed_matrix = normalize_count_df(trimmed_data_df, "log10")
plot_heatmap(normed_matrix)
calculated_dist_matrix = jsd_matrix(trimmed_data_df)
plot_heatmap(calculated_dist_matrix)
clustering_data = kmedoid_clustering(calculated_dist_matrix, 11, 500)
plot_clustering_graphs(calculated_dist_matrix, clustering_data, 11, 500)




