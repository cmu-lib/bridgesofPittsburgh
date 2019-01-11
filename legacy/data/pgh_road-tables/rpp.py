# -*- coding: utf-8 -*-
"""
Created on Thu Jun 21 19:17:20 2018

@author: weing
"""

import postman_problems
from postman_problems.solver import cpp
from postman_problems.solver import rpp
from postman_problems.stats import calculate_postman_solution_stats

# find RPP solution
circuit, graph = rpp(edgelist_filename='full-edgelist-river-required.csv', start_node='1')


# print solution route
for e in circuit:
    print(e)


# print solution summary stats
    
for k, v in calculate_postman_solution_stats(circuit).items():
    print(k, v)