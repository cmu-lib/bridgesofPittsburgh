# this program, when given a list of bridges with termini in one of four places (Pittsburgh City Center, the South Bank, the North Bank, or Herrs Island), generates a list of all possible routes between the bridges without repeating bridges

# load libraries
import csv
import itertools

# initial variables
riverBridges = []
validPaths = []

# read in CSV of bridges and termini into a list of tuples
with open('test_data.csv') as csvfile:
    reader = csv.DictReader(csvfile)
    for row in reader:
        rowBridge = (row['bridge_id'],row['terminus1'],row['terminus2'])
        riverBridges.append(rowBridge)

# create all possible permutations of bridges
potentialPaths = list(itertools.permutations(range(1,6),5))

# test permutations for termini continuity, e.g. if a bridge terminates on the South Bank, the next bridge cannot be a bridge between the North Bank and Pittsburgh City Center

# loop through all potential paths
for path in potentialPaths: 
    # only accept paths starting on Herrs Island because it is only accessible by one bridge
    if (path[0] == 1): 
        # first bridge in path has index 0; bridge_id starts at 1 while list/tuple indices start at 0
        firstBridge = 0 
        secondBridge = path[1] - 1
        
        # check to see if firstBridge and secondBridge share a terminus
        if (riverBridges[firstBridge][1] == riverBridges[secondBridge][1]) or (riverBridges[firstBridge][2] == riverBridges[secondBridge][1]):
            nextConnection = riverBridges[secondBridge][2]
        elif (riverBridges[firstBridge][1] == riverBridges[secondBridge][2]) or (riverBridges[firstBridge][2] == riverBridges[secondBridge][2]):
            nextConnection = riverBridges[secondBridge][1]
        else:
            nextConnection = "error"
            
        # start with third bridge of the path, repeat for the number of bridges in the path
        i = 2
        while (i<len(path)):
            try:
                nextBridge = path[i] - 1
                if (riverBridges[nextBridge][1] == nextConnection):
                    nextConnection = riverBridges[nextBridge][2]
                elif (riverBridges[nextBridge][2] == nextConnection):
                    nextConnection = riverBridges[nextBridge][1]
                else:
                    nextConnection = "error"
                i=i+1
            except:
                i=i+1
        
        # if looped through entire path without error, append to validPaths list
        if (nextConnection != "error"):
            validPaths.append(path)
    
# output a CSV of bridge orderings
with open('valid_network_paths.csv','w') as csvfile:
    pathWriter = csv.writer(csvfile)
    pathWriter.writerow(validPaths)