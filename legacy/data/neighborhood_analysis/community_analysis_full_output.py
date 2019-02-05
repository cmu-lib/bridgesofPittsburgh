# this code is intended to compute the Jaccard index of the Pittsburgh neighborhoods (distance between modularity classes from Gephi's algorithms and the formal neighborhoods)
# note: jaccard_similiarity_score from sklearn.metrics only functions over sets of the same size, which is not the case for our data

import csv

importFile = open("pgh_roads_by_neighborhood_modularity_only.csv")
importReader = csv.reader(importFile)	
exportFile = open("JaccardIndices.csv", "w")
exportFile.write("modularity" + "," + "hood_no" + "," + "mod_set length" + "," + "hood_set length" + "," + "intersection length" + "," + "union length" + "," + "jaccard index" + "\r\n")

# initializing index variables and empty lists to iterate through the modularity numbers (0-67) and neighborhood numbers (1-90)
mod_i = 0
hood_i = 1
mod_list = []
hood_list = []

while mod_i < 68:
	hood_i = 1
	while hood_i < 91:
	
		# construct a list of street intersections that have modularity number mod_i and neighborhood number hood_i
		for row in importReader:
			if row[3] == str(mod_i):
				list.append(mod_list, row[2])
			if row[4] == str(hood_i):
				list.append(hood_list, row[2])
		print("Just finished modularity class " + str(mod_i) + " and neighborhood " + str(hood_i))
		
		# calculate the Jaccard index and print to exportFile
		intersection_length = len(set(mod_list) & set(hood_list))
		union_length = len(set(mod_list) | set(hood_list))
		if union_length != 0:
			jaccard_index = intersection_length / union_length
		else: #this should never happen but is here for error-checking purposes
			jaccard_index = "nan"
		exportFile.write(str(mod_i) + "," + str(hood_i) + "," + str(len(mod_list)) + "," + str(len(hood_list)) + "," + str(intersection_length) + "," + str(union_length) + "," + str(jaccard_index) + "\r\n")
		
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