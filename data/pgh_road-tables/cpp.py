# -*- coding: utf-8 -*-
"""
Created on Thu Jun 21 19:17:20 2018

@author: weing
"""

import postman_problems
from postman_problems.solver import cpp
from postman_problems.stats import calculate_postman_solution_stats

# find CPP solution
circuitSmall, graphSmall = cpp(edgelist_filename='component-connections-edgelist-reduced-degree-2.csv', start_node='1')
circuit, graph = cpp(edgelist_filename='component-connections-edgelist.csv', start_node='1')


# print solution route
for e in circuit:
    print(e)

# print solution route
for e in circuitSmall:
    print(e)


# print solution summary stats
for k, v in calculate_postman_solution_stats(circuitSmall).items():
    print(k, v)
    
for k, v in calculate_postman_solution_stats(circuit).items():
    print(k, v)