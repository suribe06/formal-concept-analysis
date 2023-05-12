import os
import time
import snap
import numpy as np
import pandas as pd
import networkx as nx
import matplotlib.pyplot as plt

BASE_DIR = os.getcwd()
GR_DIR = os.path.join(BASE_DIR, 'Graphs')

def getIntervals(metric_data):
    mean_ = np.mean(metric_data)
    std_ = np.std(metric_data)
    lower_bound = mean_ - std_
    upper_bound = mean_ + std_
    clasification = []
    for x in metric_data:
        if x < lower_bound: clasification.append("low")
        elif x > upper_bound: clasification.append("high")
        else: clasification.append("medium")
    return clasification

def centralityMeasureAnalysisNX(weighted=True):
    """
    This function uses the networkx library to obtain the centrality metrics of the graph.
    The weighted parameter determines whether or not a weighted graph is used for the calculation of the centrality measures
    """
    GRAPH_FILE = os.path.join(GR_DIR, f'graph_skipgrams.gml')
    G_nx = nx.read_gml(GRAPH_FILE, label='id')
    node_mapper = {node: G_nx.nodes[node]["name"] for node in G_nx.nodes}
    # Take the n most important nodes, in this case degree are used as decision criterion
    n = 15
    top_nodes = [node[0] for node in sorted(G_nx.degree, key=lambda x: x[1], reverse=True)][:n]
    start = time.time()
    # Centrality measures
    start = time.time()
    if weighted:
        closeness = nx.closeness_centrality(G_nx, distance='weight')
        eigenvector = nx.eigenvector_centrality_numpy(G_nx, weight='weight')
        betweenness = nx.betweenness_centrality(G_nx, weight='weight')
        clustering = nx.clustering(G_nx, weight='weight')
    else:
        closeness = nx.closeness_centrality(G_nx)
        eigenvector = nx.eigenvector_centrality_numpy(G_nx)
        betweenness = nx.betweenness_centrality(G_nx)
        clustering = nx.clustering(G_nx)
    end = time.time()
    print("execution time for networkx algorithms:", end - start)
    # Obtain the metrics of the top n nodes
    top_closeness = {node: closeness[node] for node in top_nodes}
    top_eigenvector = {node: eigenvector[node] for node in top_nodes}
    top_betweenness = {node: betweenness[node] for node in top_nodes}
    top_clustering = {node: clustering[node] for node in top_nodes}
    # Convert metrics to cathegorical data
    node_names = [node_mapper[node] for node in top_nodes]
    cl_top_closeness = getIntervals(list(top_closeness.values()))
    cl_top_eigenvector = getIntervals(list(top_eigenvector.values()))
    cl_top_betweenness = getIntervals(list(top_betweenness.values()))
    cl_top_clustering = getIntervals(list(top_clustering.values()))
    # Save the data in csv
    columns_ = ['node', 'closeness', 'eigenvector', 'betweenness', 'clustering']
    df = pd.DataFrame(list(zip(node_names, cl_top_closeness, cl_top_eigenvector,
                    cl_top_betweenness, cl_top_clustering)), columns=columns_)
    df.to_csv('lattice_data.csv', index=False)
    return

def networkxToSnap(G):
    SG = snap.TUNGraph.New()
    for u in list(G.nodes):
        SG.AddNode(int(u))
    for (u, v) in G.edges():
        SG.AddEdge(int(u), int(v))
    return SG

def gitUpdate(commit):
    command_add = "git add ."
    command_commit = f"git commit -m '{commit}'"
    command_push = "git push"
    os.system(command_add)
    os.system(command_commit)
    os.system(command_push)

def centralityMeasureAnalysisSNAP():
    """
    This function uses the snap library to obtain the centrality metrics of the graph. 
    The graph used is undirected and unweighted.
    """
    GRAPH_FILE = os.path.join(GR_DIR, f'graph_skipgrams.gml')
    G_nx = nx.read_gml(GRAPH_FILE, label='id')
    node_mapper = {node: G_nx.nodes[node]["name"] for node in G_nx.nodes}
    # Convert networkx graph to snap graph
    G = networkxToSnap(G_nx)
    N = G.GetNodes()
    nodes = G.Nodes()
    # Take the n most important nodes, in this case eigenvalues are used as decision criterion
    n = 15
    eigenvector = np.zeros(N, dtype=float)
    NIdEigenH = G.GetEigenVectorCentr()
    for NI in nodes:
        node_id = NI.GetId()
        eigenvector[node_id] = NIdEigenH[node_id]
    top_nodes = sorted(list(zip(range(N), eigenvector)), key=lambda x: x[1], reverse=True)[:n]
    top_node_ids = [node[0] for node in top_nodes]
    # Lists to store centrality measures
    closeness = np.zeros(N, dtype=float)
    betweenness = np.zeros(N, dtype=float)
    farness = np.zeros(N, dtype=float)
    eccentricity = np.zeros(N, dtype=float)
    clustering = np.zeros(N, dtype=float)
    degree = np.zeros(N, dtype=float)
    # Centrality measures
    NodesBet, EdgesBet = G.GetBetweennessCentr(1.0)
    for NI in nodes:
        node_id = NI.GetId()
        closeness[node_id] = G.GetClosenessCentr(node_id)
        betweenness[node_id] = NodesBet[node_id]
        farness[node_id] = G.GetFarnessCentr(node_id)
        eccentricity[node_id] = G.GetNodeEcc(node_id, False)
        clustering[node_id] = G.GetNodeClustCf(node_id)
        degree[node_id] = G.GetDegreeCentr(node_id)
    # Obtain the metrics of the top n nodes
    top_closeness = {node: closeness[node] for node in top_node_ids}
    top_betweenness = {node: betweenness[node] for node in top_node_ids}
    top_farness = {node: farness[node] for node in top_node_ids}
    top_eccentricity = {node: eccentricity[node] for node in top_node_ids}
    top_clustering = {node: clustering[node] for node in top_node_ids}
    top_degree = {node: degree[node] for node in top_node_ids}
    # Convert metrics to cathegorical data
    node_names = [node_mapper[node] for node in top_node_ids]
    cl_top_closeness = getIntervals(list(top_closeness.values()))
    cl_top_betweenness = getIntervals(list(top_betweenness.values()))
    cl_top_farness = getIntervals(list(top_farness.values()))
    cl_top_eccentricity = getIntervals(list(top_eccentricity.values()))
    cl_top_clustering = getIntervals(list(top_clustering.values()))
    cl_top_degree = getIntervals(list(top_degree.values()))
    columns_ = ['node', 'closeness', 'betweenness', 'farness', 'eccentricity', 'clustering', 'degree']
    df = pd.DataFrame(list(zip(node_names, cl_top_closeness, cl_top_betweenness, cl_top_farness,
                    cl_top_eccentricity, cl_top_clustering, cl_top_degree)), columns=columns_)
    df.to_csv('lattice_data_2.csv', index=False)
    gitUpdate('update lattice data')
    return

def getInducedSubGraph():
    GRAPH_FILE = os.path.join(GR_DIR, f'graph_skipgrams.gml')
    G_nx = nx.read_gml(GRAPH_FILE, label='id')
    node_mapper = {node: G_nx.nodes[node]["name"] for node in G_nx.nodes}
    # Convert networkx graph to snap graph
    G = networkxToSnap(G_nx)
    N = G.GetNodes()
    nodes = G.Nodes()
    # Take the n most important nodes, in this case eigenvalues are used as decision criterion
    n = 15
    eigenvector = np.zeros(N, dtype=float)
    NIdEigenH = G.GetEigenVectorCentr()
    for NI in nodes:
        node_id = NI.GetId()
        eigenvector[node_id] = NIdEigenH[node_id]
    top_nodes = sorted(list(zip(range(N), eigenvector)), key=lambda x: x[1], reverse=True)[:n]
    top_node_ids = [node[0] for node in top_nodes]
    print(top_node_ids)
    H = G.ConvertSubGraph(snap.TUNGraph, top_node_ids)
    print(f'nodes {H.GetNodes()} edges {H.GetEdges()}')
    H.SaveEdgeList('mygraph.txt')
    names = [node_mapper[x] for x in top_node_ids]
    new_labels = dict(zip(top_node_ids, names))
    print(new_labels)
    H.SaveGViz("eigenvector.dot", "Top Eigenvector Nodes", new_labels)
    degree = np.zeros(N, dtype=float)
    for NI in nodes:
        node_id = NI.GetId()
        degree[node_id] = G.GetDegreeCentr(node_id)
    top_nodes = sorted(list(zip(range(N), degree)), key=lambda x: x[1], reverse=True)[:n]
    top_node_ids = [node[0] for node in top_nodes]
    print(top_node_ids)
    H = G.ConvertSubGraph(snap.TUNGraph, top_node_ids)
    print(f'nodes {H.GetNodes()} edges {H.GetEdges()}')
    #H.SaveEdgeList('mygraph.txt')
    names = [node_mapper[x] for x in top_node_ids]
    new_labels = dict(zip(top_node_ids, names))
    print(new_labels)
    H.SaveGViz("degree.dot", "Top Degree Nodes", new_labels)

#centralityMeasureAnalysisNX(False)
centralityMeasureAnalysisSNAP()
#getInducedSubGraph()