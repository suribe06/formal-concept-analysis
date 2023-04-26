#include <iostream>
#include <fstream>
#include "Snap.h"

int main()
{
    PNGraph Graph = TNGraph::New();
    Graph->AddNode(1);
    Graph->AddNode(5);
    Graph->AddNode(32);
    Graph->AddEdge(1,5);
    Graph->AddEdge(5,1);
    Graph->AddEdge(5,32);
    int NumNodes = Graph->GetNodes();
    int NumEdges = Graph->GetEdges();
    std::cout << "Number of edges: " << NumEdges << std::endl;
    return 0;
}