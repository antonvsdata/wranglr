## Wranglr

Wranglr is a Shiny web app that automates creation of worklist files for liquid chromatography-mass spectrometry (LC-MS) experiments. Wranglr was developed in University of Eastern Finland (UEF), mainly for the purposes of the research group of nutritional metabolomics. The online version of Wranglr is maintained by the Bioinformatics Centre of UEF.

**NOTE:** the online version of Wranglr is only meant for internal use at UEF, but the source code of Wranglr is available for anyone to use. 

#### What is Shiny?

Shiny is an R package developed by RStudio, that allows the user to create interactive web applications without knowledge of actual web development tools and languages. For more information, visit the [Shiny website](https://shiny.rstudio.com/). 

### What does Wranglr do exactly?

The software used to run LC-MS experiments requires so called worklist files.

When designing an LC-MS experiment, the researcher needs to create a so called worklist file for the software used to operate the LC-MS instrument.  These files include at least an identifier for each sample, the injection order, information about where the instrument can find the sample (i.e. the location on a well plate) and the name for the file where the raw data for that sample should be saved. To avoid systematic bias in the resulting data, the injection order of the samples should be randomized.

Creating the worklists manually (e.g. in Excel) is both time-consuming and error-prone (i.e. more time-consuming). This is where Wranglr comes in. The main tasks of Wranglr are:

- Randomizing sample injection order. Samples can also be randomized so that samples from the same group/organ/subject are run sequentially, but the samples inside the group/organ/subject are randomized.
- Assigning positions for each sample on a well-plate
- Creating identifiers for each sample
- Creating data file names for each sample
- Adding quality control (QC) samples
- Assigning samples for AutoMSMS


### How can I try Wranglr?

There are three ways you can try Wranglr. Two of them require installing R to your computer. The current version of Wranlgr is built with R version 3.5.3.

#### The online trial version

Wranglr is currently hosted at https://antonvsdata.shinyapps.io/wranglr/. You can try the app there. Please read the instructions that can be downloaded from the Instructions tab.

Note that the free instance of shinyapps.io only allows for 25 hours of use per month, so this instance of Wranglr might be unavailable. If this is the case, you must check out the other options.

#### Running Wranglr locally with shiny::runGitHub

Shiny apps have a really useful feature: you can run apps that are stored in GitHub! To do this, simply open an R session and follow these steps:

1. Make sure you have Shiny installed. If not, run ```install.packages("shiny")```
2. Install all the other packages required by Wranlgr. The below code will install all the missing packages:  
```
pckgs <- c("dplyr", "tidyr", "shiny", "openxlsx", "stringr")
  for (pckg in pckgs){
    if(!requireNamespace(pckg, quietly = TRUE)) {
      install.packages(pckg)
    }
  }
```
3. You should now have everything you need for Wranglr. Start the app by running: ```shiny::runGitHub("wranglr", "antonvsdata")```
4. See the instructions on the Instructions tab. You can use example data found there as well.

Note that you might get weird error messages if your versions of the packages do not match those used while developing Wranglr. To make sure you have the correct package versions, see the next option:

#### Cloning the repository and installing packages with packrat

This is the most reliable method for trying out Wranglr. Wranglr is a packrat project. [Packrat](https://rstudio.github.io/packrat/) is a package management tool for R, that allows you to create a local library for R packages for this project only. It also allows you to install the same package versions that were used to construct Wranglr. So, to try Wranlgr out locally, you can follow these steps:

1. Download the git repository by running the following line in your terminal:  
```git clone https://github.com/antonvsdata/wranglr```
2. Open the Wranglr.Rproj file in RStudio (or start R in the Wranglr repository if you're not using RStudio)
3. If you don't have packrat installed, it should be installed automatically. If this does not happen for some reason, install it manually ```install.packages("packrat")```. And the run ```packrat::on()```
4. To restore the packages used to create Wranglr, run ```packrat::restore()```. This will install all the required packages. Note that this might take a while.
5. To run the application, open either ui.R or server.R file in RStudio and click "Run App" in the top panel.
6. You can now test the app with the files found in testdata. See the instructions provided in ```www/Wranglr_instructions.pdf```