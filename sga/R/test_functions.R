rm(list=ls())
library(sga)
library(data.table)

df <- haven::read_stata("sourcedatasetCHX.dta")
setDT(df)
df[sex==1, sex:=2]
df[sex==0, sex:=1]
df[,weight:=weight*1000]

d <- data.table::data.table(weight=df[["weight"]],
                            sex = df[["sex"]],
                            gestage=df[["gestage"]]
)

dfixed = sga:::fix_outliers(d,igs())
growth_standard = igs()

old <- growth_standard[dfixed, on = .(g_start<gestage, g_end>=gestage)]
new <- growth_standard[dfixed, on = .(g_start<=gestage, g_end>gestage)]

df[, sga:=estimate_sga_intergrowth(df,include_outliers = T, useold=TRUE)]
df[, sgawo:=estimate_sga_intergrowth(df,include_outliers = T,useold=FALSE)]

df[,.N, .(sga,sgawo)]


# sga     N
# 1: SGA 3-10%  4870
# 2:       AGA 13474
# 3:   SGA <3%  6853
# 4:      <NA>  2166
# > df[,.N,sga_wo]
# sga_wo     N
# 1: SGA 3-10%  5154
# 2:       AGA 13768
# 3:   SGA <3%  7529
# 4:      <NA>   912
