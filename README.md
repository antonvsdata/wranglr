## Wranglr

Wranglr is a Shiny web app that automates creation of worklist files for liquid chromatography-mass spectrometry (LC-MS) experiments. Wranglr was developed in University of Eastern Finland (UEF), mainly for the purposes of the research group of nutritional metabolomics. The online version of Wranglr is maintained by the Bioinformatics Centre of UEF.

**NOTE:** the online version of Wranglr is only meant for internal use at UEF, but the source code of Wranglr is available for anyone to use. 

#### What is Shiny?

Shiny is an R package developed by RStudio, that allows the user to create interactive web applications without knowledge of actual web development tools and languages. For more information, visit the [Shiny website](https://shiny.rstudio.com/). 

#### What does Wranglr do exactly?

The software used to run LC-MS experiments requires so called worklist files.

When designing an LC-MS experiment, the researcher needs to create a so called worklist file for the software used to operate the LC-MS instrument.  These files include at least an identifier for each sample, the injection order, information about where the instrument can find the sample (i.e. the location on a well plate) and the name for the file where the raw data for that sample should be saved. To avoid systematic bias in the resulting data, the injection order of the samples should be randomized.

Creating the worklists manually (e.g. in Excel) is both time-consuming and error-prone (i.e. more time-consuming). This is where Wranglr comes in. The main tasks of Wranglr are:

- Randomizing sample injection order. Samples can also be randomized so that samples from the same group/organ/subject are run sequentially, but the samples inside the group/organ/subject are randomized.
- Assigning positions for each sample on a well-plate
- Creating identifiers for each sample
- Creating data file names for each sample
- Adding quality control (QC) samples
- Assigning samples for AutoMSMS


#### How can I try Wranglr?

**NOTE:** The current version of Wranglr is incompatible with newer versions of Shiny. This will hopefully get fixed soon!

Even though Wranglr is a web app, you can run it locally from your machine if you install all the required packages. Wranglr is a packrat project. [Packrat](https://rstudio.github.io/packrat/) is a package management tool for R, that allows you to create a local library for R packages for this project only. It also allows you to install the same package versions that were used to construct Wranglr. So, to try Wranlgr out locally, you can follow these steps:

1. Download the git repository by running the following line in your terminal:  
```git clone https://github.com/antonvsdata/wranglr```
2. Open the Wranglr.Rproj file in RStudio (or start R in the Wranglr repository if you're not using RStudio)
3. If you don't have packrat installed, it should be installed automatically. If this does not happen for some reason, install it manually ```install.packages("packrat")```. And the run ```packrat::on()```
4. To restore the packages used to create Wranglr, run ```packrat::restiore()```. This will install all the required packages. Note that this might take a while.
5. To run the application, open either ui.R or server.R file in RStudio and click "Run App" in the top panel.
6. You can now test the app with the files found in testdata. See the instructions provided in ```www/Wranglr_instructions.pdf```