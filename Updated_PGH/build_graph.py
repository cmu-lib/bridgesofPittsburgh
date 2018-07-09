import csv
import networkx as nx


with open('edges.csv','r') as lineData:
    lineData = csv.reader(lineData)
    headers = next(lineData)
    edges = [row for row in lineData]

def idData(i):
    return headers.index(i)

mg = nx.MultiDiGraph()
for edge in edges:
    f = edge[idData("from_")]
    t = edge[idData("to")]
    if(edge[idData("oneway")]=='False'):
        mg.add_edge(f, t)
        mg.add_edge(t, f)
    else:
        mg.add_edge(f, t)

print("Node count: %d" % mg.number_of_nodes())
print("Edge count: %d" % mg.number_of_edges())