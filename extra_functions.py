import os
import random
import pandas as pd
import networkx as nx

BASE_DIR = os.getcwd()
GR_DIR = os.path.join(BASE_DIR, 'Graphs')

def graph2CSV():
    GRAPH_FILE = os.path.join(GR_DIR, f'graph_skipgrams.gml')
    G_nx = nx.read_gml(GRAPH_FILE, label='id')
    print(f'nodos {G_nx.number_of_nodes()} arcos {G_nx.number_of_edges()}')
    n_sample = 7000
    random.seed(56)
    sample_nodes = random.sample(list(G_nx.nodes), n_sample)
    H = G_nx.subgraph(sample_nodes)
    filename = 'edge_list.csv'
    nx.write_edgelist(G_nx, os.path.join(GR_DIR, filename), delimiter=',', data=['weight'])

def generateLatticeFile():
    df = pd.read_csv('lattice_data.csv')
    labels = ['low', 'medium', 'high']
    df_new = pd.DataFrame()
    df_new['node'] = df['node']
    attributes = list(df.columns)
    attributes.pop(0)
    for attr in attributes:
        for l in labels:
            col = attr + '_' + l
            df_new[col] = 0  # Initialize with zeros
    # Assign 1 to the corresponding columns according to the labels
    for i in range(len(df)):
        for attr in attributes:
            val = df.loc[i, attr]
            col = attr + '_' + val
            df_new.loc[i, col] = 1
    df_new = df_new.rename(columns={'node': ''})
    df_new.to_csv('new_lattice_data.csv', sep=';', index=False)

#graph2CSV()
#generateLatticeFile()