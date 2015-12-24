library(plyr)
library(reshape2)
library(ggplot2)
theme_set(theme_bw())

## fdf = read.csv("exports/fees-2015-02-06.csv")
## fudf = read.csv("exports/feeusages-2015-02-06.csv")

## Load invoices dataframe
idf = read.csv(
  "invoice.csv",
  colClasses=c("character", "character", "character", "character", "character",
               "character", "character", "character")
)
idf = rename(idf, c("key" = "invoice_id"))
idf$submitted = as.POSIXct(idf$submitted, format="%Y-%m-%dT%H:%M:%S")

## Get only the last invoice submitted for each bill
idflast = ddply(idf, .(file_id), function(df) {
  df[which.max(df$submitted),]
})

## Provider frequency by site
ddply(idflast[,c("site", "provider")], .(site, provider), count)

## Load usages dataframe
udf = read.csv(
  "usage.csv",
  colClasses=c("character", "character", "character", "character", "Date",
               "character", "character", "numeric")
)
udf = rename(udf, c("parent" = "invoice_id"))
udf$end[udf$end == ""] = NA
udf = udf[udf$start < "2016-01-01",]
udf = udf[udf$service != "Oil Additional Services",]
udf = udf[!(udf$service == ""),]
udf$service[udf$service == "e"] = "Electricity"
udf$service[udf$service == "Natural Gas"] = "Fuel"
udf$service[udf$service == "Propane"] = "Fuel"
udf$service[udf$service == "Oil"] = "Fuel"
udf$service[udf$service == "Stormwater "] = "Water"
udf$service[udf$service == "Waste Water"] = "Water"
udf$service[udf$service == "Street Light"] = "Electricity"
udf$end = as.Date(udf$end)

## Merge invoices with usages
iudf = merge(idflast, udf, by=c('invoice_id'))

iudf$site[iudf$site == "Hill House"] = "HIL"
iudf$site[iudf$site == "Mill City"] = "MCM"
iudf$site[iudf$site == "MN Historical Society"] = "INSTI"
iudf$site[iudf$site == "1500 Mississippi"] = "1500"
iudf$site[iudf$site == "Grand Mound"] = "GMD"
iudf$site[iudf$site == "Mille Lacs Indian Museum"] = "MLM"
iudf$site[iudf$site == "Minnesota History Center"] = "MHC"
iudf$site[iudf$site == "Fort Snelling"] = "FTS"
iudf$site[iudf$site == "Kelley Farm"] = "KLF"
iudf$site[iudf$site == "Ramsey House"] = "RAM"
iudf$site[iudf$site == "Sibley House"] = "SIB"
iudf$site[iudf$site == "Forest History Center"] = "FHC"
iudf$site[iudf$site == "Lindbergh Historic Site"] = "LHS"
iudf$site[iudf$site == "Jeffers Petroglyghs"] = "JPG"
iudf$site[iudf$site == "Split Rock Lighthouse"] = "SRL"
iudf$site[iudf$site == "Northwest Fur Post"] = "NWC"
iudf$site[iudf$site == "Birch Coulee Battlefield"] = "BCB"
iudf$site[iudf$site == "Historic Forestville "] = "HFV"
iudf$site[iudf$site == "Comstock House"] = "COM"
iudf$site[iudf$site == "Mayo House"] = "MAY"
iudf$site[iudf$site == "Fort Ridgley + Harkin Store"] = "FTR & HAR"

## Service uses by site
ddply(iudf[,c("site", "service")], .(site, service), count)

min = min(na.omit(iudf$start))
max = max(na.omit(iudf$start))

ggplot(iudf) +
  # geom_segment(aes(x=start, xend=end, y=site, yend=site), size=3, alpha=0.25) +
  geom_segment(aes(x=start, xend=start + 10, y=site, yend=site, color=site), size=2) +
  geom_blank(aes(x=iudf$start)) +
  #  scale_colour_brewer() +
  #  scale_x_date(limits=c(min, max)) +
  facet_grid(service ~ ., scales = "free_y", space="free")
ggsave("service_coverage.pdf", width=12, height=8)

ggplot(iudf) +
  geom_segment(aes(x=start, xend=end, y=provider, yend=provider), size=3, alpha=0.25) +
  geom_segment(aes(x=start, xend=start + 2, y=provider, yend=provider, color=is.na(end)), size=2) +
  geom_blank(aes(x=iudf$start)) +
  scale_colour_brewer(palette="Set1") +
  #  scale_x_date(breaks="4 months", limits=c(min, max)) +
  facet_wrap(~ site, ncol=1, scales="free")
ggsave("provider_coverage.pdf", width=12, height=25)

