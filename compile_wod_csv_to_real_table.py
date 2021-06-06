#!/usr/bin/env python
#
# compile_wod_csv_to_real_table.py

'''compile_wod_csv_to_real_table.py  last modified 2021-06-07
    combine multiple WOD csv-format files into an actual table

    data were downloaded from
  https://www.ncei.noaa.gov/access/world-ocean-database-select/dbsearch.html

    the source csv description is found at
  https://www.ncei.noaa.gov/access/world-ocean-database-select/csv_info.html

compile_wod_csv_to_real_table.py -c *.csv.gz > ocldb1616358314.25649.OSD_all.all_vars.tab

    processing required 22Gb RAM, and took 16 minutes

'''

import sys
import argparse
import gzip
import time
from collections import defaultdict


example_cast = """
#--------------------------------------------------------------------------------,
CAST                        ,,        9540582,WOD Unique Cast Number,WOD code,
NODC Cruise ID              ,,GB-11003       ,,,
Originators Station ID      ,,               ,,,alpha,
Originators Cruise ID       ,,               ,,,
Latitude                    ,,         -55.13,decimal degrees,,
Longitude                   ,,           22.0,decimal degrees,,
Year                        ,,           1772,,,
Month                       ,,             12,,,
Day                         ,,             15,,,
METADATA,
Country                     ,,             GB,NODC code,GREAT BRITAIN
Accession Number            ,,           571.,NODC code,,
Platform                    ,,          8152.,OCL code,HMS RESOLUTION (Launched 1770;ex.HMS Drake 12.1771),
probe_type                  ,,             7.,OCL_code,bottle/rosette/net,
Database origin             ,,             3.,WOD code,GODAR Project,
 Discrete Water Sampler     ,,           775.,Bottle, model and brand unkno,Hale apparatus with thermometer (Prestwich, 1875),
VARIABLES ,Depth     ,F,O,Temperatur ,F,O,,
UNITS     ,m         , , ,degrees C ,, , ,,
Prof-Flag ,          ,0, ,          ,0, ,,
         1,        0.,0, ,      -1.1,0, ,
         2,      183.,0, ,       1.1,0, ,
END OF VARIABLES SECTION,

BIOLOGY METADATA,,
Mesh size                   ,,           64.0,microns,,
Gear                        ,,           105.,WOD code,Juday Net (Tropical or Large),
Net mouth area              ,,         0.1257,m2,,
Preservation method         ,,            18.,WOD code,formalin, no info on: %,buffering,water type,
BIOLOGY,Upper Z,Lower Z,Measuremnt Type,ORIGINAL VALUE ,F,Orig unit,WOD CBV value  ,F,_unit,_meth,WOD PGC,ITIS TSN,mod,sex,lif,trp,rlm,ftr,spm  ,min size  ,max size  ,ind length,ind width ,ind radius,tsv       ,,,
1,15.,30.,,8.,0,,,,,,2080000,TINTINNOPSIS MEUNIERI,,,,,,,,,,,,,
2,30.,50.,,8.,0,,,,,,2080000,TINTINNOPSIS MEUNIERI,,,,,,,,,,,,,
3,50.,100.,,8.,0,,,,,,2080000,TINTINNOPSIS MEUNIERI,,,,,,,,,,,,,
4,0.,13.,,8.,0,,,,,,2080000,PARAFAVELLA ACUTA,,,,,,,,,,,,,
5,30.,50.,,7.,0,,,,,,2080000,PARAFAVELLA ACUTA,,,,,,,,,,,,,
--
90,0.,13.,,7.,0,,,,,,4212010,PSEUDOCALANUS,MODIFIER=1.,SEX=2.,,,,,,,,,,,
91,0.,13.,,8.,0,,,,,,4212010,CENTROPAGES,MODIFIER=1.,,LIFE STAGE=7.,,,,,,,,,,
92,0.,13.,,7.,0,,,,,,4212010,TEMORA,MODIFIER=1.,,LIFE STAGE=7.,,,,,,,,,,
93,0.,13.,,8.,0,,,,,,4212010,ACARTIA LONGIREMIS,,,,,,,,,,,,,
94,0.,13.,,8.,0,,,,,,4212010,ACARTIA,MODIFIER=1.,,LIFE STAGE=7.,,,,,,,,,,
END OF BIOLOGY SECTION,

#--------------------------------------------------------------------------------						
CAST                        		9540582	WOD Unique Cast Number	WOD code		
NODC Cruise ID              		GB-11003       				
Originators Station ID      		               			alpha	
Originators Cruise ID       		               				
Latitude                    		-55.13	decimal degrees			
Longitude                   		22	decimal degrees			
Year                        		1772				
Month                       		12				
Day                         		15				
METADATA						
Country                     		             GB	NODC code	GREAT BRITAIN		
Accession Number            		571	NODC code			
Platform                    		8152	OCL code	HMS RESOLUTION (Launched 1770;ex.HMS Drake 12.1771)		
probe_type                  		7	OCL_code	bottle/rosette/net		
Database origin             		3	WOD code	GODAR Project		
 Discrete Water Sampler     		775	Bottle	 model and brand unkno	Hale apparatus with thermometer (Prestwich	 1875)
VARIABLES 	Depth     	F	O	Temperatur 	F	O
UNITS     	m         	 	 	degrees C 		 
Prof-Flag 	          	0	 	          	0	 
1	0	0	 	-1.1	0	 
2	183	0	 	1.1	0	 
END OF VARIABLES SECTION						
"""

def F_to_C_conversion(Ftemp):
	Ctemp = (Ftemp - 32) * 5 / 9
	return Ctemp

def main(argv, wayout):
	if not len(argv):
		argv.append("-h")
	parser = argparse.ArgumentParser(formatter_class=argparse.RawDescriptionHelpFormatter, description=__doc__)
	parser.add_argument('-b','--get-biology', action="store_true", help="keep the BIOLOGY METADATA and info")
	parser.add_argument('-c','--csv', nargs="*", help="WOD csv-format files")
	parser.add_argument('-f','--keep-flags', action="store_true", help="get variable flag info i.e. columns F and O for each variable")
	parser.add_argument('--list-headers', action="store_true", help="list all headers for metadata and their frequency")
	parser.add_argument('--list-variables', action="store_true", help="list all variable classes and their frequency")
	args = parser.parse_args(argv)

	sys.stderr.write("#DEBUG: keep F and O flags for variables: {}\n".format(args.keep_flags) )
	sys.stderr.write("#DEBUG: track counts of CSV row/column headers: {}\n".format(args.list_headers) )
	sys.stderr.write("#DEBUG: track counts of each variable: {}\n".format(args.list_variables) )

	# these will store data to be printed
	cast_data = defaultdict(dict) # key is cast_id, value is dict of all cast data
	cast_metadata = defaultdict(dict) # key is cast_id, value is dict of remaining metadata for country etc
	cast_variables = defaultdict(dict) # key is cast_id, value is 

	# these are for debugging, to count how often certain data features are used
	header_counter = defaultdict(int)
	all_variables = defaultdict(int) # key is a variable name, value is count of number of times it is used

	# begin loop on each csv file
	for csvfile in args.csv:

		if csvfile.rsplit('.',1)[-1]=="gz": # autodetect gzip format
			opentype = gzip.open
			sys.stderr.write("# Parsing csv from {} as gzipped  {}\n".format(csvfile, time.asctime() ) )
		else: # otherwise assume normal open for fasta format
			opentype = open
			sys.stderr.write("# Parsing csv from {}  {}\n".format(csvfile, time.asctime() ) )

		linecount = 0
		entrycount = 0
		count_headers = False
		get_variables = False
		get_biology = args.get_biology

		for line in opentype(csvfile, 'rt'):
			line = line.strip()
			if line: # skip blank lines
				linecount += 1

				# reset at new entry
				if line[0]=="#":
					count_headers = True
					entrycount += 1
					cast_id = None
					cruise_id = "NA"
					orig_station_id = ""
					orig_cruise_id = ""
					latitude = ""
					longitude = ""
					year = 0
					month = 0
					day = 0
					country = ""
					acc_number = ""
					platform = ""
					probe_type = ""
					database_origin = ""
					water_sampler = ""
					variable_data = defaultdict(list) # key is variable (e.g. Depth), value is list of stations in order
					variable_names = [] # list of names in order that they appear in the VARIABLES row
					continue

				# split at comma for CSV
				lsplits = line.split(',')
				header = lsplits[0].strip()
				if count_headers is True:
					header_counter[header] += 1

				# assign based on the different headers
				if header == "CAST":
					cast_id = lsplits[2].strip()
				elif header == "NODC Cruise ID":
					cruise_id = lsplits[2].strip()
				elif header == "Originators Station ID":
					orig_station_id = lsplits[2].replace("#", "no.").replace("'","").strip()
				elif header == "Originators Cruise ID":
					orig_cruise_id = lsplits[2].replace("'","").strip()
				elif header == "Latitude":
					latitude = lsplits[2].strip()
				elif header == "Longitude":
					longitude = lsplits[2].strip()
				elif header == "Year":
					year = lsplits[2].strip()
				elif header == "Month":
					month = lsplits[2].strip()
				elif header == "Day":
					day = lsplits[2].strip()
				elif header == "METADATA":
					pass
				elif header == "Country":
					country = lsplits[2].strip()
				elif header == "Accession Number":
					acc_number = lsplits[2].replace(".","").strip()
				elif header == "Platform":
					platform = lsplits[2].strip()
				elif header == "probe_type":
					probe_type = lsplits[2].strip()
				elif header == "Database origin":
					database_origin = lsplits[2].strip()
				elif header == "Discrete Water Sampler":
					water_sampler = lsplits[2].strip()
				elif header == "VARIABLES":
					variable_headers = [ v.strip() for v in lsplits[1:] if v.strip() ]
					current_variable = ""
					for i, variable in enumerate(variable_headers):
						variable = variable.strip()
						if i % 3 == 0: # meaning a main column
							current_variable = variable
							var_name = variable
						else: # meaning join F and O columns with variable name before
							var_name = "{}_{}".format( current_variable, variable)
						variable_names.append( var_name )
						all_variables[var_name] += 1
				elif header == "UNITS":
					pass
				elif header == "Prof-Flag": # meaning the next lines are variables
					get_variables = True
					count_headers = False
				elif header == "END OF VARIABLES SECTION":
					get_variables = False
					if cast_id is None:
						raise TypeError("ERROR: entry {} in {} has no cast_id\n".format(entrycount, csvfile) )
					cast_info_data_dict = {"cast_id":cast_id , "cruise_id":cruise_id , "orig_station_id":orig_station_id , "orig_cruise_id":orig_cruise_id , "latitude":latitude , "longitude":longitude , "year":year , "month":month , "day":day}
					cast_data[cast_id] = cast_info_data_dict
					cast_metadata_dict = { "country":country, "acc_number":acc_number }
					cast_metadata[cast_id] = cast_metadata_dict
					cast_variables[cast_id] = variable_data
				elif get_variables is True: # assume it is now the list of depths
					for j, var_name in enumerate(variable_names):
						variable_data[var_name].append( lsplits[j+1].strip() )
				elif header == "BIOLOGY METADATA":
					pass # TODO possibly process this
				elif header == "BIOLOGY":
					count_headers = False
				else: # for all other headers not coded
					pass
		sys.stderr.write("# Counted {} entries over {} lines from {}  {}\n".format( entrycount, linecount, csvfile, time.asctime() ) )
		# end of file loop
	sys.stderr.write("# Counted {} casts  {}\n".format( len(cast_data), time.asctime() ) )


	# begin printing table
	sys.stderr.write("# Writing table to file  {}\n".format( time.asctime() ) )

	# generate header line
	variables_no_flags = [v for v in all_variables.keys() if v.count("_")==0 and v!="Depth"]
	cast_data_headers = ["cast_id", "cruise_id", "orig_station_id", "orig_cruise_id", "latitude", "longitude", "year", "month", "day"]
	table_header = cast_data_headers + [ "country", "country_acc_number", "stop", "depth"] + variables_no_flags
	sys.stdout.write("{}\n".format( "\t".join(table_header) ) )

	# iterate through data dicts and print one line for each stop
	stop_counter = 0
	for cast_id in cast_data.keys():
		base_output_line = [] # this will be blank, and use the first columns for each stop per cast
		# write metadata for each line
		for field in cast_data_headers:
			base_output_line.append( cast_data.get(cast_id).get(field,"NA") )
		for field in [ "country", "acc_number" ]:
			base_output_line.append( cast_metadata.get(cast_id).get(field,"NA") )
		depth_list = cast_variables.get(cast_id).get("Depth") # should be list
		for j, depth in enumerate(depth_list): # iterate through stops within each cast
			stop_counter += 1
			output_depth = depth
			if output_depth[-1]==".": # some depths end in . make this .0
				output_depth += "0"
			var_output_line = [j, output_depth]
			for var in variables_no_flags: # get values or NA for all variables for each stop
				try:
					var_value = cast_variables.get(cast_id).get(var, [] )[j]
					if var_value[-1]==".": # some variables end in . make this .0
						var_value += "0"
				except IndexError:
					var_value = "NA"
				if var_value=="---*---" or var_value=="**********": # no idea where this comes from, should be NA
					var_value = "NA"
				var_output_line.append( var_value )
			final_output_line = list( map( str, base_output_line + var_output_line) )
			sys.stdout.write("{}\n".format( "\t".join(final_output_line) ) )
	sys.stderr.write("# Counted {} stops for {} total casts from {} files  {}\n".format( stop_counter, len(cast_data), len(args.csv), time.asctime() ) )

	# for debugging, print all header info categories that were found
	if args.list_headers:
		for header_name, header_count in sorted(header_counter.items(), key=lambda x: x[1], reverse=True):
			sys.stderr.write("#HED:{}\t{}\n".format(header_name, header_count) )
	# for debugging, print all possible variable categories
	if args.list_variables:
		for var_name, var_count in sorted(all_variables.items(), key=lambda x: x[1], reverse=True):
			sys.stderr.write("#VAR:{}\t{}\n".format(var_name, var_count) )

	# finally over

if __name__ == "__main__":
	main(sys.argv[1:],sys.stdout)
