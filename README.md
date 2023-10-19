# Agilent-MS-MS-Flat-File-Generation
Application to convert the MSMS spectra files from exported spectra from Agilent MassHunter Qualitative Analysis to a flat file format written in R and using the Shiny package to create a user friendly interface.

The application allows one to collect spectral information from Agilent LC-MS instruments using software applications from the Agilent software suite (Agilent MassHunter Qualitative Analysis) and the programing language R with RStudio. The process uses a custom R script with UI to allow extraction of MS data from the proprietary Agilent spectral files and conversion to open-source spectral formats. The output is in the form of text files containing the spectral information of each respective compound, collision energy and other parameters specific to the spectra of the compound.

The Agilent Spectra Acquisition App.R takes one to multiple .csv files exported spectra (converted from profile to centroid before exporting) from the Agilent MassHunter Qualitative Analysis software program. The user has the option to provide additional information from the instrument in the form of a excel file that acts ass a template for other parameters from the instrument not stored in the database file created using mzVault.

To smoothly run the R script, the user should use an R environment like R Studio and create a folder for the accompanying R files and parameter template file. Creating a new R project using the newly created folder will also help consolidate the output text files.

To run the R script with the user inputs, open the “Run Agilent Spectra Acquisition App.R” and select Run App in the top right corner of the Script page. 

A Rshiny menu should appear where the user can browse for the database file and pre-filled template file created before or the user can write in the spectral parameters in the optional text boxes in the user interface. the user also has the option to turn on mass defect filtering so only masses with a negative mass defect will retained in the output. This filter is useful for heavily flourinated compounds and the script was orignally created to aide in the data anlayis of PFAS compounds. The other optional filter the user can turn on is a filter to remove masses with a relative intensity below 100 to reduce noise in the spectra.
