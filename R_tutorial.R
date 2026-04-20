example_data = data.table(year=2005:2007, value = 1:3)

x = 7
y = 4
z = x+y

#############

vec_numbers = c(1,2,3,4)

vec_characters <- c("A", "B", "word")

vec_numbers_alt = c(5,6,9,10)

#############

table = data.table(
  gender=c("Male", "Female"),
  value =c(10, 13)
)

table[, .(
  ratio = value[gender=="Female"]/value[gender=="Male"]
)]

gender = c("Male", "Female")
value = c(10, 13)
value[gender=="Female"]/value[gender=="Male"]

###############

dt = data.table(
  
  category=c(rep("Gender", 6L), rep("Race", 9L), "Age"),
  group = c(rep(c("Male", "Female"), 3L), rep(c("White", "Black", "Hispanic"), 3L), "23"),
  average = c(10,13,  11,13,  12,14,  
              14,14,14,  15,14,13,  14,16,17,
              10),
  
  year = c(2010,2010, 2011,2011, 2012,2012, 2010,2010,2010, 2011,2011,2011, 2012,2012,2012, 2010)
)

###############
dt[, column_of_ones := 1]

dt[, column_of_ones_long := c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)]

dt[group=="23", one := 1]

dt[category=="Gender", 
   F_M_ratio := average[gender=="Female"]/average[gender=="Male"],
  year]

################
DT = data.table(
  x=c(1,2,3,7,8),
  y=c(2,2,2,3,4)
)

DT[, z := x+y]
DT[, w := x-y]
# The ':='( just means everything inside the parentheses gets created, and we no longer need the colon, just the = sign. It can save space in your file.
DT[, ':='(u = x*y, v = x/y)]
DT[, log_y := log(y)]

DT[, mean_x := mean(x)]
DT[, var_z := var(z)]

mean_x_value = mean(DT[, x])
var_z_value = var(DT[, z])

DT[, ':='(mean_x=mean_x_value, var_z=var_z_value)]

################
DT = data.table(
  year=c(2024,2024,2024,2024, 2025,2025,2025,2025),
  month=c(1,1,12,12,  1,1,12,12),
  county=c("Hennepin","Ramsey","Hennepin","Ramsey",  "Hennepin","Ramsey","Hennepin","Scott"),
  
  x=1:8
)

DT[, mean_year_x := mean(x), by = year]

DT[, mean_year_month_x := mean(x), by = .(year, month)]
DT[, mean_year_month_county_x := mean(x), by = .(year, month, county)]

###############
DT[, mean(x), by = year]

DT[, .(mean_year_x = mean(x)), by = year]
DT[, .(mean_x_year_county = mean(x)), by = .(year, county)]

DT[, y := 5:12]

DT[, .(mean_year_x = mean(x), mean_year_y = mean(y)), by = year]
DT[, .(mean_year_x = mean(x), var_year_x = var(x)), by = year]

############### Examples and Practice
## Remark) The details here (just the setup) might change if the source of the data is not to be my combined files
pctls = fread("//rb.win.frb.org/I1/Accounts/J-L/i1jzk01/Redirected/Desktop/IDDA/combined_data/percentiles_combined.csv")

## Sample restrictions:
  # PAW - TC
  # "xredXsex" for BM and MF v. WM
  # US
data = pctls[samp == "prime_age_working_w2" & inc_var == "TC" & group_var=="xredXsex" & 
      group_var_val%in%c("NH_White_Male","NH_Black_Male","NH_Black_Female") &
      geo_var == "usst" & 25 <= pctl & pctl <= 99.9
  ]
data_male = data[group_var_val%in%c("NH_White_Male","NH_Black_Male"), 
                  .(ratio = value[group_var_val=="NH_Black_Male"]/value[group_var_val=="NH_White_Male"]),
                  by = .(year, pctl)]
data_female = data[group_var_val%in%c("NH_White_Male","NH_Black_Female"), 
                  .(ratio = value[group_var_val=="NH_Black_Female"]/value[group_var_val=="NH_White_Male"]),
                  by = .(year, pctl)]