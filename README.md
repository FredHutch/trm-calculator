
![](./app/www/trmHeader.png?raw=true)

Welcome to the GitHub repository for the Fred Hutchinson Cancer Center's Treatment-Related Mortality Calculator. You can access this application at the following **link** (URL to be included).

### Background
Treatment-Related Mortality (TRM) refers to patient death resulting from medication or treatment. TRM is commonly considered when evaluating treatment options for patients with acute myeloid leukemia (AML). Calculating a patient's risk of TRM through the use of a variety of factors such as age and blood-related lab measurements can help guide decision making for AML treatment protocols.

The Fred Hutch TRM calculator makes use of regression models developed by [Walter et al.](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5404219/) to quickly evaluate TRM probability and risk for patients diagnosed with AML. This [R Shiny](https://shiny.posit.co) application serves as the replacement to a [previously existing tool](https://trmcalculator.fredhutch.org) used by clinicians at Fred Hutch, and we anticipate adding more features that they would like to use side-by-side in the future.

### Access
This application lives at the **following link** (URL to be included) and can be accessed freely by the public. You can also run this Shiny application locally by downloading the source code available in this repository - the following packages will need to be installed in your R environment prior to running the application:

```
shiny; 
shinythemes; 
shinydashboard; 
shinyvalidate; 
dplyr; 
gt; 
gtExtras;
```

### Contact
This project is managed by the Fred Hutch Data Science Lab (DaSL)'s [Translational Analytics](https://hutchdatascience.org/tr-analytics/) team. You can read more about our group, mission, and other work on the [DaSL homepage](https://hutchdatascience.org).

To report any bugs, submit patches, or request new features, please log an issue [in our issue tracker](https://github.com/FredHutch/trm-calculator/issues/new). For direct inquiries, please send an email to DaSL Translational Analytics at [analytics\@fredhutch.org](mailto:analytics@fredhutch.org){.email}.
