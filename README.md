# Replication Code for Identifying Price Informativeness

The main project folder contains five subfolders and fourteen R source/markdown files. The import data subfolder has two R source files.

Instructions:

1. Run the files 01_stocks_import.R, 02_stocks_import_inst_ownership.R, and 03_move_files_to_input_raw.cmd in the import_data folder.
   - The file 03_move_files_to_input_raw.cmd simply moves the output files to the input/data_raw folder. It only works for windows. In other OS's, simply move the files manually.

2. Run the files in the main project folder sequentially. Alternatively, simply run the file 00_stocks_main.R.

Comments:

- A complete run of the code generates all the figure and tables in the paper with the exception of those with public signals.
- The results with public signals are obtained by uncommenting the variables controls_levels and controls_logs in the file 04a_stocks_recover.Rmd and running the code gain.
- The simulation folder includes the simulations in Figures 6, OA-15 and OA-16.s

**The files rolling_a.csv and rolling_q.csv include the estimated rolling measures of price informativeness by stock (permno).**
