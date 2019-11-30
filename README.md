# Code and data from "Monitoring hydropower reliability in Malawi with satellite data and machine learning"

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0) [![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://GitHub.com/Naereen/StrapDown.js/graphs/commit-activity)

**This repository hosts:**

 - Python files to interact with the Google Earth Engine API and process the remotely sensed data
 - A R script interacting with those Python files via the reticulate package and carrying out the analysis
 - Supporting data to run the analysis 
 
**To replicate the analysis, the following steps should be followed:**
 
- Retrieve SPEI drought index nc files from  https://feemit-my.sharepoint.com/:f:/g/personal/giacomo_falchetta_feem_it/ElWS_wYKxoxDuaKDW2FZpbIBK83onBoz4ixmqMDtoOeemg?e=w1iV0j (too large to be hosted on GitHub) and insert them in root directory of the repository.
- Make sure Python 3.x+ is available on the local machine and the dependencies listed in the python_dependencies.txt have been installed.
- Open the Script.R file and replace the paths to the repository (line 68) and to the Python executable (line 13). 
- Ensure the required R dependencies are installed.
- Launch the script. 

Source code-related issues should be opened directly on the "Issues" tab. Broader questions of the methods should be addressed to giacomo.falchetta@feem.it




