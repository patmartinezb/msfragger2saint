## msfragger2saint

This tool launches a local Shiny-based web application that generates the three files required by SAINT (http://proteomics.fi/), as a .zip file. The input can be either a single file (reprint.spc) or several ones that can be combined into one. The app also provides a general visualization of the data.


## **Requirements for the input file**

A .tsv file.

The the output of FragPipe, usually named as "reprint.spc", which means that it has to include the following annotation columns: "PROTID", "GENEID", and "PROTLEN"; and the columns with the **actual data** (remember to keep the names as simple as possible). On the second line of the file, there should be the metadata / experiment information, as it was written on the Workflow tab of FragPipe.


> For the app to work, it is **mandatory** that this information is present. In addition, keep in mind that, if the file/s have many entries, it will take a bit of time to generate the .zip file
