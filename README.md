## Minimalistic Open Online Course as a Shiny App

Define with an Rmd file a simple shiny app that mixes Videos and multiple choice quizzes.

**Work in progress not yet well documented or stable. Functions may still change.**

### Installation

```r
if (!require(remotes))
  install.packages("remotes")
  
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")

remotes::install_github("skranz/stringtools")
remotes::install_github("skranz/rmdtools")
remotes::install_github("skranz/shinyEvents")
remotes::install_github("skranz/miniMOOC")
```

### Example code to preview a miniMOOC Rmd file

```r
library(miniMOOC)
preview_mooc_rmd("myfile.Rmd")
```

### Special miniMOOC Rmd tags

Start a new section by adding the line.
```
#. section
```
Each section will be shown as a separate tab.


Insert a youtube video whose id (last part of video url is `youtubeid`) by adding the line
```
#. youtube id="youtubeid"
```

Example to add a multiple choice quiz:

```
#< quiz "emma_rational"
question: |
  Could Emma's prices in the data reasonably have been her profit maximizing prices?
sc:
  - yes*
  - no
success: |
  Correct. Some people may too quickly interpret the data such that higher prices *cause* higher sales. Then Emma's prices would indeed have been too low. But the graph does only show a *relationship* between prices and sold amount, not a causal effect (and not a demand function). Indeed, we have simulated the data from a model with a downward sloping demand function in which Emma sets profit maximizing prices. We will explain this below.
#>
```
The `success` field is optional. The correct solution is marked with a `*` at the end.

