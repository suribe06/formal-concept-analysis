import os
import networkx as nx
import random

BASE_DIR = os.getcwd()
GR_DIR = os.path.join(BASE_DIR, 'Graphs')

GRAPH_FILE = os.path.join(GR_DIR, f'graph_skipgrams.gml')
G_nx = nx.read_gml(GRAPH_FILE, label='id')

print(f'nodos {G_nx.number_of_nodes()} arcos {G_nx.number_of_edges()}')

n_sample = 7000
random.seed(56)
sample_nodes = random.sample(list(G_nx.nodes), n_sample)
H = G_nx.subgraph(sample_nodes)
print(f'nodos {H.number_of_nodes()} arcos {H.number_of_edges()}')

nx.write_edgelist(G_nx, 'induced.csv', delimiter=',', data=['weight'])