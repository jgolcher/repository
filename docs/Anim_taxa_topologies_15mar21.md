Animated ggplot illustrating a central problem in phylogenetics
================

How many possible tree configurations or tree topologies?
---------------------------------------------------------

To know how closely related are a set of taxa, we need to consider the rapid increment in probable topologies.For instance, 50 taxa translate into 2.752921e+76 rooted trees or 2.838063e+74 unrooted trees. Estimating the true topology from all the possible configurations of taxa in phylogenies is computationally challenging and thus requires optimization of search strategies.

First we will run the formula estimating total numbers of unrooted versus rooted trees for n number of taxa:

``` r
#Step 1: Turn formula to estimate the number of unrooted/rooted trees into functions
funrooted<-function(n)
{
  factorial(2*n-5)/((2^(n-3))*factorial(n-3))
}
frooted<-function(n)
{
  factorial(2*n-3)/((2^(n-2))*factorial(n-2))
}
#frooted(50) #try out a number
#funrooted(50)
```

Visualizing this problem in phylogenetics as an animated ggplot
---------------------------------------------------------------

If we simulate a sequence of numbers and plot using the functions above: the rapid increase in the numbers of trees with the number of taxa looks weird. What happens is that there is wide variation in the values for y that the majority of points then collapse near zero, leaving one big gap between the first and second y axis tick mark. An animated figure showing the process that leads to this is then easier to interpret. Here is how I have approached this issue using the R package gganimate:

``` r
#load packages
library("reshape2") 
library("ggplot2")
library("gganimate")
library("magick")
```

    ## Linking to ImageMagick 6.9.11.57
    ## Enabled features: cairo, fontconfig, freetype, heic, lcms, pango, raw, rsvg, webp
    ## Disabled features: fftw, ghostscript, x11

``` r
#Simulate data
x <- seq(3,50,1)
#create simple data frame
df<-data.frame(taxa=x, unrooted_trees=funrooted(x),rooted_trees=frooted(x))
#turn wide df into a long format for plotting purposes
df_long <- melt(df, id="taxa")
#Establish scientific notation
options(scipen=10)
#ggplot
p <- ggplot(df_long, aes(x=taxa, y=value,color=variable))+
  geom_line( color="gray", lty="dashed") +
  labs(y = "Number of trees", x = "Number of taxa") +
  geom_text(aes(label=value),hjust=0, vjust=0)+
  scale_color_manual(values = c("unrooted_trees" = "#2A00B6", "rooted_trees" = "#E94657",name = "")) +
  geom_point(aes(group = seq_along(variable)), size = 3, alpha = 0.7,shape=21)+
  theme_bw() +
  theme(legend.title=element_blank(),plot.title = element_text(size = 24, family = "Avenir"),
        text = element_text(size = 24,family = "Avenir"))
```

``` r
##Animated ggPlot
p2<-p +transition_reveal(along = taxa) + view_follow(fixed_x = TRUE)
p2
```

![](Anim_taxa_topologies_15mar21_files/figure-markdown_github/unnamed-chunk-2-1.gif)

``` r
#save
#anim<-animate(p2,height = 800, width =800)
#magick::image_write(anim, path="trees.gif")
```

Note that the values of y, or numbers of possible tree topologies dynamically appear as do the data points and the y axis changes depending on the data!

    ## 
    ## To cite reshape2 in publications use:
    ## 
    ##   Hadley Wickham (2007). Reshaping Data with the reshape Package.
    ##   Journal of Statistical Software, 21(12), 1-20. URL
    ##   http://www.jstatsoft.org/v21/i12/.
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Article{,
    ##     title = {Reshaping Data with the {reshape} Package},
    ##     author = {Hadley Wickham},
    ##     journal = {Journal of Statistical Software},
    ##     year = {2007},
    ##     volume = {21},
    ##     number = {12},
    ##     pages = {1--20},
    ##     url = {http://www.jstatsoft.org/v21/i12/},
    ##   }

    ## 
    ## To cite ggplot2 in publications, please use:
    ## 
    ##   H. Wickham. ggplot2: Elegant Graphics for Data Analysis.
    ##   Springer-Verlag New York, 2016.
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Book{,
    ##     author = {Hadley Wickham},
    ##     title = {ggplot2: Elegant Graphics for Data Analysis},
    ##     publisher = {Springer-Verlag New York},
    ##     year = {2016},
    ##     isbn = {978-3-319-24277-4},
    ##     url = {https://ggplot2.tidyverse.org},
    ##   }

    ## 
    ## To cite package 'gganimate' in publications use:
    ## 
    ##   Thomas Lin Pedersen and David Robinson (2020). gganimate: A Grammar
    ##   of Animated Graphics. R package version 1.0.7.
    ##   https://CRAN.R-project.org/package=gganimate
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {gganimate: A Grammar of Animated Graphics},
    ##     author = {Thomas Lin Pedersen and David Robinson},
    ##     year = {2020},
    ##     note = {R package version 1.0.7},
    ##     url = {https://CRAN.R-project.org/package=gganimate},
    ##   }

    ## 
    ## To cite package 'magick' in publications use:
    ## 
    ##   Jeroen Ooms (2021). magick: Advanced Graphics and Image-Processing in
    ##   R. R package version 2.7.0. https://CRAN.R-project.org/package=magick
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {magick: Advanced Graphics and Image-Processing in R},
    ##     author = {Jeroen Ooms},
    ##     year = {2021},
    ##     note = {R package version 2.7.0},
    ##     url = {https://CRAN.R-project.org/package=magick},
    ##   }

    ## 
    ## To cite R in publications use:
    ## 
    ##   R Core Team (2020). R: A language and environment for statistical
    ##   computing. R Foundation for Statistical Computing, Vienna, Austria.
    ##   URL https://www.R-project.org/.
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {R: A Language and Environment for Statistical Computing},
    ##     author = {{R Core Team}},
    ##     organization = {R Foundation for Statistical Computing},
    ##     address = {Vienna, Austria},
    ##     year = {2020},
    ##     url = {https://www.R-project.org/},
    ##   }
    ## 
    ## We have invested a lot of time and effort in creating R, please cite it
    ## when using it for data analysis. See also 'citation("pkgname")' for
    ## citing R packages.
