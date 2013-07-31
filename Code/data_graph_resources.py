# I want to see if I can somehow graph the connections between different 
# codes in the dataset to see th ecompany structure

# This code scales sizes of n1 nodes according to counts

import networkx as nx
import matplotlib.pyplot as plt
import csv

# Read the csv file and skip the first row
f = open("./../data/train.csv",'r')
ff = csv.reader(f)
ff.next()

# Initialize data types
nodes = []
edges = []
col = dict() # dictionary of colors for each n1 node
count = dict()
av_action = dict()
size = [] # sizes for n1 nodes
cols = [] # color list used in drawing nodes
		  # for secondary node the colors will be set to red if average action is 1,
		  # and green if average action is <1
		  # main nodes are blue

# Choose which columns to plot
# main nodes - department etc
c1 = 9
# secondary nodes - resource or manager
c2 = 1

# How many rows of code to read?
# The graph plot is fuzzy if the number is too big
max_row = 1000


i = 0
for line in ff:
	# get nodes from two columns
	n1 = line[c2]+'m'
	n2 = line[c1]+'d'
	
	# append lists of nodes
	nodes.append(n1)
	nodes.append(n2)
	# Assign colors to nodes in a color dictionary
	col[n1] = "r"
	col[n2] = "b"

	# Add edges
	edges.append((n1,n2))


	# count number of times the n1 was used
	if n1 in count.keys():
		count[n1] += 1.
	else:
		count[n1] = 1.

	# get the action for each n1
	action = line[0]
	if n1 in av_action.keys():
		av_action[n1] += float(action)
	else:
		av_action[n1] = float(action)

	
	# If i > maxrow, stop reading the file	
	i += 1
	if i >= max_row:
		break

# Average action for each n1
for k in av_action.keys():
	av_action[k] = av_action[k]/count[k]


# Initialize graph
G=nx.Graph()

# Add nodes and edges
G.add_nodes_from(nodes)
G.add_edges_from(edges)




# calculate colors and sizes for each n1 node
# size is given by count
for n in G.nodes():
	if n in av_action.keys():
		size.append(count[n]*20)
		if av_action[n] == 1:
			cols.append("r")
		else:
			cols.append('g')
	
	else:
		cols.append("b")
		size.append(50)

# draw a graphviz layout
nx.draw_graphviz(G,node_color=cols,with_labels=False,node_size=size)
plt.show()


