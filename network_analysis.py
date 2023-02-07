import snap
import os
import networkx as nx
import matplotlib.pyplot as plt

BASE_DIR = os.getcwd()
GR_DIR = os.path.join(BASE_DIR, 'Graphs')

num_articles = 9

def getGraphPerArticle(num_articles):
    for i in range(num_articles):
        GRAPH_FILE = os.path.join(GR_DIR, f'graph_skipgrams_{i}.gml')
        G_nx = nx.read_gml(GRAPH_FILE, label='id')
        # Get the 10 nodes with the highest degree
        n = 10
        top_nodes = [node[0] for node in sorted(G_nx.degree, key=lambda x: x[1], reverse=True)][:n]
        # Create the induced subgraph
        G_induced = G_nx.subgraph(top_nodes)
        # Plot the graph
        pos = nx.spring_layout(G_induced, k=0.5, iterations=50)
        plt.clf()
        nx.draw(G_induced, pos, with_labels=False, node_size=500)
        labels = nx.get_node_attributes(G_induced, 'name')
        nx.draw_networkx_labels(G_induced, pos, labels, font_size=12)
        # Add edge weights to the plot
        edge_labels = {(u, v): G_induced[u][v]['weight'] for u, v in G_induced.edges}
        #nx.draw_networkx_edge_labels(G_induced, pos, edge_labels=edge_labels)
        plt.savefig(f"graph_plot_{i}.png")

def getGraphUnion(num_articles):
    G_combined = nx.Graph()
    # Dictionary to store the mapping between old node IDs and new node IDs
    node_labels = {}
    for i in range(num_articles):
        GRAPH_FILE = os.path.join(GR_DIR, f'graph_skipgrams_{i}.gml')
        G_nx = nx.read_gml(GRAPH_FILE, label='id')
        # Get the 10 nodes with the highest degree
        n = 10
        top_nodes = [node[0] for node in sorted(G_nx.degree, key=lambda x: x[1], reverse=True)][:n]
        # Create the induced subgraph
        G_induced = G_nx.subgraph(top_nodes)
        # Loop through all the nodes in the induced subgraph
        for node, data in G_induced.nodes(data=True):
            # Get the node name
            node_name = data['name']
            # Check if node name is not in the mapping
            if node_name not in node_labels:
                # If not, create a new node ID and add the node name to the mapping
                node_id = len(node_labels)
                node_labels[node_name] = node_id
                # Add the node to the combined graph with the node ID and name
                G_combined.add_node(node_id, name=node_name)
            else:
                # If node name is already in the mapping, get the node ID
                node_id = node_labels[node_name] 
            # Iterate over neighbors and edge data for the current node
            for neighbor, edge_data in G_induced[node].items():
                # Get the neighbor name
                neighbor_name = G_induced.nodes[neighbor]['name']
                # Check if neighbor name is not in the mapping
                if neighbor_name not in node_labels:
                    # If not, create a new node ID and add the neighbor name to the mapping
                    neighbor_id = len(node_labels)
                    node_labels[neighbor_name] = neighbor_id
                    # Add the neighbor to the combined graph with the node ID and name
                    G_combined.add_node(neighbor_id, name=neighbor_name)
                else:
                    # If neighbor name is already in the mapping, get the node ID
                    neighbor_id = node_labels[neighbor_name]
                # Check if there is not an edge between node_id and neighbor_id in the combined graph
                if not G_combined.has_edge(node_id, neighbor_id):
                    # If not, add an edge between node_id and neighbor_id with edge data
                    G_combined.add_edge(node_id, neighbor_id, **edge_data)
    # Write the graph to file
    OUTPUT_FILE = os.path.join(GR_DIR, 'graph_union.gml')
    nx.write_gml(G_combined, OUTPUT_FILE)
    n = 10
    top_nodes = [node[0] for node in sorted(G_combined.degree, key=lambda x: x[1], reverse=True)][:n]
    # Create the induced subgraph
    G_induced = G_combined.subgraph(top_nodes)
    # Plot the graph
    pos = nx.circular_layout(G_induced)
    nx.draw(G_induced, pos, with_labels=False, node_size=500)
    labels = nx.get_node_attributes(G_induced, 'name')
    nx.draw_networkx_labels(G_induced, pos, labels, font_size=12)
    plt.show()
    return

def getSuperGraph():
    GRAPH_FILE = os.path.join(GR_DIR, f'graph.gml')
    G_nx = nx.read_gml(GRAPH_FILE, label='id')
    # Get the 10 nodes with the highest degree
    n = 10
    top_nodes = [node[0] for node in sorted(G_nx.degree, key=lambda x: x[1], reverse=True)][:n]
    # Create the induced subgraph
    G_induced = G_nx.subgraph(top_nodes)
    # Write the graph to file
    OUTPUT_FILE = os.path.join(GR_DIR, 'super_graph.gml')
    nx.write_gml(G_induced, OUTPUT_FILE)
    # Plot the graph
    pos = nx.circular_layout(G_induced)
    nx.draw(G_induced, pos, with_labels=False, node_size=500)
    labels = nx.get_node_attributes(G_induced, 'name')
    nx.draw_networkx_labels(G_induced, pos, labels, font_size=12)
    plt.show()


#getGraphPerArticle(num_articles)
getGraphUnion(num_articles)
getSuperGraph()