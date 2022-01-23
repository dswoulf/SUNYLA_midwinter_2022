# SUNYLA_midwinter_2022

While preparing to publish datasets, I ran into a problem: how can I make information in hard-to-understand and specialized metadata structures more accessible for potential users/reusers of the data? And can this solution help solve data deluge problems from multiple sources like ILS systems, repositories, web statistics like Google Analytics, usage statistics, program attendance, etc that can overwhelm users? My answer was to take an old approach to this relatively new problem by using literate programming, a methodology defined by Donald Knuth in 1984 that integrates a programming language and at least one document markup language in the same file, to generate replicable and versionable documentation and reports. This presentation will be useful for anyone who benefits from assessment reports or deals with data/metadata in a library. Attendees will be given access to this Github repository with examples along with resources that they can bring back to their institution to generate their own custom reports as websites, word documents, PDFs, and other formats.

## Resources

**Literate programming**
- Data Carpentry's Reproducible Research Literate Programming lessons (<https://datacarpentry.org/rr-literate-programming/>)
- <http://www.literateprogramming.com/>

**R and Rmarkdown**

- R Tutorial (<https://www.w3schools.com/r/>) 
- R Markdown: The definitive guide (<https://bookdown.org/yihui/rmarkdown/>)
- Useful packages to parse metadata and data: readr, xml2, jsonlite, yaml

**Python and Jupyter Notebook**

- Python Tutorial (<https://www.w3schools.com/python/>) 
- Jupyter Tutorial (<https://www.tutorialspoint.com/jupyter/index.htm>) 

**Markup languages**

- Markdown cheat sheet (<https://www.markdownguide.org/cheat-sheet/>) 
- A simple guide to LaTeX – step by step (<https://latex-tutorial.com/tutorials/>) 
- HTML tutorial (<https://www.w3schools.com/html/>) 
- CSS Tutorial (<https://www.w3schools.com/css/>) 

## Instructions to produce PDFs in .Rmd files

*Prerequisites*: Have [pandoc](https://pandoc.org/installing.html) and LaTeX installed on your computer. LaTeX can be installed using the tinytex() package in RStudio or by going to <https://www.latex-project.org/get/>.

1. Specify pdf_document in the header
2. Have xml2 package installed in R. For other formats use the jsonlite package for JSON files or the yaml package for YAML files
3. Create a new Rmarkdown file in RStudio. 
4. Load xml2 library using “library(xml2)”. Read in the XML file that’s being converted and access different elements using xml2 functions (https://xml2.r-lib.org/reference/index.html).
5. Create the document being sure to include the markup. Knit, view the document, make changes and iterate until you’re happy.