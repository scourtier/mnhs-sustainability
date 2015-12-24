require(data.table)

## load usages
usage = fread("utilities_data/datastore_export/usage.csv", stringsAsFactors=TRUE,
              colClasses=c(parent="factor", key="factor"))
setnames(usage, c("parent", "key"), c("invoice_key", "usage_key"))
setkey(usage, invoice_key, usage_key)

## raw usage count by service and unit
table(usage[, .(unit, service)], useNA="ifany")

## recode service and units
## NOTE: "Waste Water" is coded NA (to be removed later) to avoid double-counting
levels(usage$service) = c(
  NA, "Electricity", "Natural Gas", "Oil", NA, "Propane", "Water", "Electricity",
  "Trash", NA, "Water", "Electricity")
levels(usage$unit) = c(
  NA, "cu ft", "gal", "T", "W", NA, "cu ft", "cu ft", NA, NA, NA, "gal",
  "gal", "gal", "gal", "kW", "kWh", "kW", "kWh", "lbs", NA, NA, NA, NA, "thm",
  "thm", "T", "W", NA, "yd", "yd")
table(usage[, .(unit, service)], useNA="ifany")

## load fees
fee = fread("utilities_data/datastore_export/fee.csv", stringsAsFactors=TRUE,
            colClasses=c(parent="factor", key="factor"))
setnames(fee, c("parent", "key"), c("invoice_key", "fee_key"))
setkey(fee, invoice_key, fee_key)

## load feeRusage
feeRusage = fread("utilities_data/datastore_export/feeRusage.csv", stringsAsFactors=TRUE,
                  colClasses="factor")
setkey(feeRusage, invoice_key, fee_key)

## overall fees by service by year
usage[
  setkey(feeRusage[
    fee
  ][
  , .(usage_key, amount=amount / .N), by=key(fee)
  ],
  invoice_key, usage_key)
][
, .(amount=sum(amount)), by=.(start_date=as.integer(strftime(start, "%Y")), service)
][
  order(service, start_date)
]

## omit Trash and NA usages, drop usages with rate units, then drop unused unit levels
usage = na.omit(usage)
usage = usage[service != "Trash" & unit != "kW" & unit !="W"]
usage = droplevels(usage)

## convert units and drop unused unit levels
cuFtToGal = function(x) 7.48052 * x
usage[unit == "cu ft", `:=`(quantity=cuFtToGal(quantity), unit="gal")]
galGasToThm = function(x) 1.49828783357 * x # assuming residential fuel oil
usage[service == "Natural Gas" & unit == "gal",
      `:=`(quantity=galGasToThm(quantity), unit="thm")]
usage = droplevels(usage)

## overall usage by service/unit by year
usage[
, .(quantity=sum(quantity)),
  by=.(start_date=as.integer(strftime(start, "%Y")),
       service,
       unit)
][
  order(service, start_date)
]
