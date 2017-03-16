# Rdataviz2017

This repository contains a slide deck entitled "Advanced R Data Visualization," which was presented to UC Berkeley Epidemiology PhD students on 16 March 2016.  Included in the presentation are examples of the use of [ggplot2](http://cran.r-project.org/web/packages/ggplot2/index.html) and a subset of the interactive graphic authoring packages currently available for R. The deck was built using the [R](http://www.r-project.org/) package [xaringan](https://github.com/yihui/xaringan), with support from [knitr](http://cran.r-project.org/web/packages/knitr/index.html), [R Markdown](https://rmarkdown.rstudio.com), and [pandoc](http://johnmacfarlane.net/pandoc/).  Extensions employed in this deck include:
+ the [remark.js](https://github.com/gnab/remark/) framework,
+ the [RefManageR](http://cran.r-project.org/web/packages/RefManageR/index.html) bibliography manager, and
+ the [highlight.js](https://highlightjs.org/) syntax highlighter.

The file for the main deck is [index.html](https://saraemoore.github.io/Rdataviz2017/index.html), built from the [RMarkdown](http://rmarkdown.rstudio.com/) file [index.Rmd](https://github.com/saraemoore/Rdataviz2017/blob/master/index.Rmd). To recompile **in R**:

```
# install.packages("rmarkdown")
library(rmarkdown)
render("index.Rmd")
browseURL("index.html")
```

**Note**: To compile as-is, the following R packages should be installed (versions used to successfully render this document are included for reference):
+ [rmarkdown](https://CRAN.R-project.org/package=rmarkdown) v1.3
+ [RefManageR](https://CRAN.R-project.org/package=RefManageR) v0.13.1
+ [WHO](https://CRAN.R-project.org/package=WHO) v0.2
+ [ggplot2](https://CRAN.R-project.org/package=ggplot2) v2.2.1
+ [sysfonts](https://CRAN.R-project.org/package=sysfonts) v0.5
+ [showtext](https://CRAN.R-project.org/package=showtext) v0.4-6
+ [dplyr](https://CRAN.R-project.org/package=dplyr) v0.5.0
+ [ggrepel](https://cran.r-project.org/package=ggrepel) v0.6.5
+ [tidyr](https://CRAN.R-project.org/package=tidyr) v0.6.1
+ [ggdendro](https://CRAN.R-project.org/package=ggdendro) v0.1-20
+ [gtable](https://CRAN.R-project.org/package=gtable) v0.2.0
+ [ggiraph](https://github.com/davidgohel/ggiraph) v0.3.2.9000 (install via `devtools::install_github()` as of this writing)
+ [gdtools](https://cran.r-project.org/package=gdtools) v0.1.3
+ [plotly](https://CRAN.R-project.org/package=plotly) v4.5.6
+ [DT](https://CRAN.R-project.org/package=DT) v0.2
+ [leaflet](https://CRAN.R-project.org/package=leaflet) v1.1.0
+ [googleVis](https://CRAN.R-project.org/package=googleVis) v0.6.2
+ [pander](https://CRAN.R-project.org/package=pander) v0.6.0

**Note**: the `googleVis` graphic to which there is a link on slide 74 requires Flash. Your browser may require additional configuration to display this graphic when working from a local repository.
