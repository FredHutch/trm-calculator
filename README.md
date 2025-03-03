
![](./app/www/trmHeader.png?raw=true)

Welcome to the GitHub repository for the Fred Hutchinson Cancer Center's Treatment-Related Mortality Calculator. You can access this application at the following link: [https://trmcalculator.fredhutch.org](https://trmcalculator.fredhutch.org).

### Background
Treatment-Related Mortality (TRM) refers to patient death resulting from medication or treatment. TRM is commonly considered when evaluating treatment options for patients with acute myeloid leukemia (AML). Calculating a patient's risk of TRM through the use of a variety of factors such as age and blood-related lab measurements can help guide decision making for AML treatment protocols.

The Fred Hutch TRM calculator makes use of regression models developed by [Walter et al.](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3221524/) to quickly evaluate TRM probability and risk for patients diagnosed with AML.

### Access
This application lives at [https://trmcalculator.fredhutch.org](https://trmcalculator.fredhutch.org) and can be accessed freely by the public. You can also run this Shiny application locally by downloading the source code available in this repository - the following packages will need to be installed in your R environment prior to running the application:

```
shiny; 
shinythemes; 
shinydashboard; 
shinyvalidate; 
gt; 
gtExtras;
bslib;
shinyBS;
```

### Contact
This project is managed by the Fred Hutch Data Science Lab (DaSL). To report any bugs, submit patches, or request new features, please log an issue [in our issue tracker](https://github.com/FredHutch/trm-calculator/issues/new). For direct inquiries, please send an email to Katie Russell at [katrus\@fredhutch.org](mailto:katrus@fredhutch.org).
