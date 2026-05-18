#### JDK, 5/18/2026
#### This file is to provide the code for the BEE students of Brandon Hawkins' post,
  # https://www.minneapolisfed.org/article/2023/the-prosperity-of-high-earning-foreign-born-workers

library(data.table)
library(ggplot2)

dir <- "C:/Users/IRJZK01/Desktop/IDDA/combined_data/"

## Get "across" shares, abbreviating it to 'a':
a <- fread(paste0(dir, "across_combined.csv"))

## Get "percentiles", abbreviating to 'pctls'"
pctls <- fread(paste0(dir, "percentiles_combined.csv"))

## (We can subset to much smaller data here.) ##

## Get some colors for our plots
light_blue = "#298FC2"



#### Plot 1: Share of entire distribution, then top 10, 1, and .1 percent that are FB in 2005 and 2019
  # (Pretty sure it is W2 TC data)
  # We are using population and not income proportions, so we use 'proportion' and not 'inc_proportion'
plot_1 = a[year%in%c(2005L,2019L) & level=="pik" & samp=="all_w2_pik" & inc_var=="TC" & geo_var=="usst" & group_var_val=="Foreign_Born" & percentile%in%c(0, 90, 99, 99.9)]
ggplot(plot_1, aes(x=year, y=proportion)) + geom_col() +
  facet_wrap(~percentile, nrow=1) +
  scale_fill_manual(
    values = c(
      "2005" = "grey70",
      "2023" = "steelblue"
    ),
    name = "Year"
  )
## Notes:
  # 'geom_col()' is the standard way to do bar charts like this.
  # We want each percentile to have 2005 and 2019 beside each other, so the facet is on 'percentile'
    # 'nrow=1' is to make it all on one row, like the website has. This is optional.
