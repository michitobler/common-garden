Instructions for getting from raw trajectories files to summary results:
-Run the noNa_video_analysis.r script
-for this script you will need all your trajectories files in one folder that is
named using the date of when the experiment was ran
-the naming convention needed for all the trajectories files is as follows:
	-mm-dd-yy_tanknumber-population-foodtreatment trajectories.txt
-this script also needs a .csv file that contains the scale for each one of the
trajectories files you are analyzing
-this script will calculate total distance in both pixels and mm, speed, and a 
running cumulative sum of distance in mm all compiled in a .csv file that is 
written to the same directory that your trajectories results are in
-after running the noNa_video_analysis script, to move forward in getting the summary statistics
run the r script open_field_data_analysis.r
-for this script to run properly, it needs access to a .xlsx file that contains
all the directories of the trajectories result files
	-these trajectories results file names should already be in the right naming
	themes if created with previously mentioned r script
-this script will output a single .csv file that contains calculations for the freezing time
of the fish, swimming time, rapid movement time, total distance in mm, max and min speeds,
and an average speed

