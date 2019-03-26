# this code is intended to compute the Jaccard index of the Pittsburgh neighborhoods (distance between modularity classes from Gephi's algorithms and the formal neighborhoods)
# note: jaccard_similiarity_score from sklearn.metrics only functions over sets of the same size, which is not the case for our data

import csv

importFile = open("intersections_with_modularity_and_neighborhoods.csv") # hard-coded csv file name
importReader = csv.reader(importFile)	
exportFile = open("JaccardIndices.csv", "w")
exportFile.write("modularity" + "," + "hood_no" + "," + "hood_name" + "," + "mod_set length" + "," + "hood_set length" + "," + "intersection length" + "," + "union length" + "," + "jaccard index" + "\r\n")

# initializing index variables and empty lists to iterate through the modularity numbers (0-89) and neighborhood numbers (1-90)
mod_i = 0
hood_i = 1
mod_list = []
hood_list = []

while mod_i < 90: # hard-coded number of modularity classes
	hood_i = 1
	while hood_i < 91:
		# construct a list of street intersections that have modularity number mod_i and neighborhood number hood_i
		for row in importReader:
			if row[4] == str(mod_i): # hard coded row with modularity class number
				list.append(mod_list, row[1]) # hard coded row with OSM id number
			if row[6] == str(hood_i): # hard coded row with Pittsburgh neighborhood id number
			    hood_name = row[5]
			    list.append(hood_list, row[1]) # hard coded row with OSM id number
		print("Just finished modularity class " + str(mod_i) + " and neighborhood " + str(hood_i))
		
		# calculate the Jaccard index and print to exportFile iff nonzero
		intersection_length = len(set(mod_list) & set(hood_list))
		union_length = len(set(mod_list) | set(hood_list))
		if union_length != 0:
			jaccard_index = intersection_length / union_length
		else: #this should never happen but is here for error-checking purposes
			jaccard_index = "nan"
		if jaccard_index != 0:
			exportFile.write(str(mod_i) + "," + str(hood_i) + "," + str(hood_name) + "," + str(len(mod_list)) + "," + str(len(hood_list)) + "," + str(intersection_length) + "," + str(union_length) + "," + str(jaccard_index) + "\r\n")
		
		# reset variables for next iteration through neighborhood numbers
		mod_list = []
		hood_list = []
		hood_i = hood_i + 1
		importFile.seek(0)
		importReader = csv.reader(importFile)
	
	#reset variables for next iteration through modularity numbers
	importFile.seek(0)
	importReader = csv.reader(importFile)
	mod_i = mod_i + 1

# close the files
importFile.close()
exportFile.close()