import os
import networkx as nx
import matplotlib.pyplot as plt
import snap as sn
import numpy as np
import pandas as pd
import random
import time

BASE_DIR = os.getcwd()
GR_DIR = os.path.join(BASE_DIR, 'Graphs')

def getIntervals(metric_data):
    mean_ = np.mean(metric_data)
    std_ = np.std(metric_data)
    lower_bound = mean_ - std_
    upper_bound = mean_ + std_
    clasification = []
    for x in metric_data:
        if x < lower_bound: clasification.append("BAJO")
        elif x > upper_bound: clasification.append("ALTO")
        else: clasification.append("MEDIO")
    return clasification

def centralityMeasureAnalysisNX():
    GRAPH_FILE = os.path.join(GR_DIR, f'graph_skipgrams.gml')
    G_nx = nx.read_gml(GRAPH_FILE, label='id')
    # Take a random sample of nodes including the n nodes with the highest degree
    n = 15
    top_nodes = [node[0] for node in sorted(G_nx.degree, key=lambda x: x[1], reverse=True)][:n]
    n_sample = 7000
    random.seed(56)
    random_nodes = random.sample(list(G_nx.nodes), n_sample)
    sample_nodes = top_nodes + random_nodes
    # Generate induced graph with selected nodes
    H = G_nx.subgraph(sample_nodes)
    H_copy = H.copy()
    # Remove nodes with 0 degree
    #isolated_nodes = list(nx.isolates(H_copy))
    #H_copy.remove_nodes_from(isolated_nodes)
    print(f'numero de nodos de grafo inducido por la muestra: {H_copy.number_of_nodes()}')
    print(f'numero de aristas de grafo inducido por la muestra: {H_copy.number_of_edges()}')
    print(nx.is_connected(H_copy))
    print(nx.number_connected_components(H_copy))
    start = time.time()
    # Centrality measures
    closeness = nx.closeness_centrality(H_copy, distance='weight')
    eigenvector = nx.eigenvector_centrality_numpy(H_copy, weight='weight')
    betweenness = nx.betweenness_centrality(H_copy, weight='weight')
    clustering = nx.clustering(H_copy, weight='weight')
    end = time.time()
    print("Tiempo de ejecucion calculo metricas:", end - start)
    # Obtain the metrics of the top n nodes with the highest degree
    top_closeness = {node: closeness[node] for node in top_nodes}
    top_eigenvector = {node: eigenvector[node] for node in top_nodes}
    top_betweenness = {node: betweenness[node] for node in top_nodes}
    top_clustering = {node: clustering[node] for node in top_nodes}
    # Convert metrics to cathegorical data
    keys_nodes = list(top_closeness.keys())
    cl_top_closeness = getIntervals(list(top_closeness.values()))
    cl_top_eigenvector = getIntervals(list(top_eigenvector.values()))
    cl_top_betweenness = getIntervals(list(top_betweenness.values()))
    cl_top_clustering = getIntervals(list(top_clustering.values()))
    # Save the data in csv
    columns_ = ['node', 'closeness', 'eigenvector', 'betweenness', 'clustering']
    df = pd.DataFrame(list(zip(keys_nodes, cl_top_closeness, cl_top_eigenvector,
                    cl_top_betweenness, cl_top_clustering)), columns=columns_)
    df.to_csv('lattice_data.csv', index=False)
    return

centralityMeasureAnalysisNX()