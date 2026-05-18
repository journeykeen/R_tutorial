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
dark_blue = "#003B5C"
gold = "#CDAF1E"


#### Plot 1: Share of entire distribution, then top 10, 1, and .1 percent that are FB in 2005 and 2019
  # (Pretty sure it is W2 TC data)
  # We are using population and not income proportions, so we use 'proportion' and not 'inc_proportion'
plot_1 = a[year%in%c(2005L,2019L) & level=="pik" & samp=="all_w2_pik" & inc_var=="TC" & geo_var=="usst" & group_var_val=="Foreign_Born" & percentile%in%c(0, 90, 99, 99.9)]
ggplot(plot_1, aes(x=year, y=proportion, fill=factor(year))) + geom_col() +
  facet_wrap(~percentile, nrow=1) + theme_minimal() +
  scale_fill_manual(
    values = c(
      "2005" = light_blue,
      "2019" = dark_blue
    ),
    name = "Year"
  )
## Notes:
  # 'geom_col()' is the standard way to do bar charts like this.
  # We want each percentile to have 2005 and 2019 beside each other, so the facet is on 'percentile'
    # 'nrow=1' is to make it all on one row, like the website has. This is optional.
  # 'fill=factor(year)' is what we need for the colors. 'factor()' around 'year' is to tell R that we are treating year as categorical,
    #   rather than continuous, as we would in a time series. 
    # The 'name' in 'scale_fill_manual' is the nickname for R. It already knows what variable 'fill' is using, 'year', since we 
    #   defined that in the aesthetics, 'aes()'


#### Plot 2: 90, 99, and 99.9 percentiles by year for FB v. U.S.-born earners. Each plot is a percentile.
  # (Pretty sure it is W2 TC data)
  # We only want the "Foreign_Born" and "Not_Foreign_Born" groups, which are all contained in the 'group_var' of "xfb".
  #   We could thus equivalently subset with 'group_val_val%in%c("Foreign_Born","Not_Foreign_Born")'.
plot_2 = pctls[level=="pik" & samp=="all_w2_pik" & inc_var=="TC" & geo_var=="usst" & group_var=="xfb" & pctl%in%c(90, 99, 99.9)]
ggplot(plot_2, aes(x=year, y=value_real, color=group_var_val)) + geom_point() + geom_line() +
  facet_wrap(~pctl, nrow=1, scale="free_y") + theme_minimal() +
  scale_color_manual(
    values = c(
      "Foreign_Born" = gold,
      "Not_Foreign_Born" = dark_blue
    ),
    name = "Group"
  )
## Notes: 
  # We are plotting lines now, so we want 'geom_point() + geom_line()'
  # In Plot 1 for each percentile (the facet) we had two years, so we used 'fill' to define a color.
    # Here, for each percentile we have groups ('group_var_val'), so we use 'color' for the colors. We do not need 'factor()', since the 
    #  variable is already non-numeric (hence R treats it as categorical already).
    # Note: 'fill' v. 'color' is delicate. Generally use 'color' for points+lines and fill for columns+bars
  # Instead of 'scale_fill_manual()' as in Plot 1 we now have 'scale_color_manual()'