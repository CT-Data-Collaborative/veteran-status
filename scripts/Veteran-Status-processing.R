library(dplyr)
library(datapkg)
library(acs)
library(stringr)
library(reshape2)
library(data.table)
library(plyr)
source('./scripts/acsHelpers.R')

##################################################################
#
# Processing Script for Veteran Status
# Created by Jenna Daly
# On 11/27/2017
#
##################################################################

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw <- (paste0(getwd(), "/", raw_location))

ctGeos <- getCTGeos()
# years for which we will process this dataset
yearList = c(2010:2016)

tn = "B21001"
acsdata <- getACSData(ctGeos, yearList=yearList, table = tn)

# Use column names from file.
columns <- fread("scripts/B21001-columns.csv")

dataset <- data.table()
for(data in acsdata) {
    year <- data@endyear
    print(paste("Processing: ", year))
    year <- paste(year-4, year, sep="-")

    # Aggregations - these columns will end up as the last 15 columns in `data`
    # and are accounted for in the `columns` list
    for (i in 0:14) {
        # aggregations by age (ie Gender = Total, by age and vet status)
        data <- cbind(
                data,
                acsSum(data, (c(7,25)+i), paste("new", i, sep="_"))
            )
    }

    # put new column names on data
    acs.colnames(data) <- columns$name

    # get fips from geography
    datafips <- data.table(fips = getACSFips(data))

    # new data tables to build
    # Data tables must be instantiated with correct number of rows
    # and we need to add this extra data anyway
    numbers <- data.table(
            FIPS = datafips$fips,
            Year = rep_len(year, nrow(data)),
            `Measure Type` = rep_len("Number", nrow(data)),
            Variable = rep_len("Veteran Status", nrow(data))
        )
    numbers.moe <- data.table(
            FIPS = datafips$fips,
            Year = rep_len(year, nrow(data)),
            `Measure Type` = rep_len("Number", nrow(data)),
            Variable = rep_len("Margins of Error", nrow(data))
        )
    props <- data.table(
        FIPS = datafips$fips,
        Year = rep_len(year, nrow(data)),
        `Measure Type` = rep_len("Percent", nrow(data)),
        Variable = rep_len("Veteran Status", nrow(data))
    )
    props.moe <- data.table(
        FIPS = datafips$fips,
        Year = rep_len(year, nrow(data)),
        `Measure Type` = rep_len("Percent", nrow(data)),
        Variable = rep_len("Margins of Error", nrow(data))
    )
    for(col in 1:ncol(data)) {
        # numeric measure
        numbers[,columns[col,name] := estimate(data[,col])]
        numbers.moe[,columns[col,name] := standard.error(data[,col])*1.645]

        # proportional measures - not calculated for the most aggregated (100%) values
        if (col > 1) {
            p <- divide.acs(data[,col], data[,1], method = "proportion")
            # i guess we have to do this manually? the method seems inconsistent
            if (any(is.nan(standard.error(p)))) {
                print("Fixing NaN")
                p <- divide.acs(data[,col], data[,1], method = "ratio")
            }
            props[,columns[col,name] := estimate(p)]
            props.moe[,columns[col,name] := standard.error(p)*1.645]
        }
    }

    numbers <- melt(
            rbind(numbers, numbers.moe),
            id.vars=c("FIPS", "Year", "Measure Type", "Variable"),
            variable.name="Column",
            variable.factor = F,
            value.name="Value",
            value.factor = F
         )
    props <- melt(
            rbind(props, props.moe),
            id.vars=c("FIPS", "Year", "Measure Type", "Variable"),
            variable.name="Column",
            variable.factor = F,
            value.name="Value",
            value.factor = F
         )

    dataset <- rbind(dataset, numbers, props)

}

#Final Additions, processing
# Split meaningful dimensions out of old column names
dataset[,c("Gender", "Age", "Veteran Status"):=do.call(Map, c(f=c, strsplit(`Column`, ":", fixed=T)))]
dataset[,`Column` := NULL]

# Round Values according to type/variable
dataset[`Measure Type` == "Number", Value := round(Value, 0)]
dataset[`Measure Type` != "Number", Value := round(Value*100, 2)]

# Join town names by FIPS code
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
towns <- (town_fips_dp$data[[1]])

dataset <- merge(towns, dataset, by = "FIPS", all.x=T)

#set final column order
dataset <- dataset %>% 
  select(Town, FIPS, Year, Age, `Veteran Status`, Gender, `Measure Type`, Variable, Value) %>% 
  arrange(Town, Year, Age, `Veteran Status`, Gender, `Measure Type`, desc(Variable))

write.table(
    dataset,
    file.path("data", "veteran-status-2016.csv"),
    sep = ",",
    row.names = F,
    na = "-9999"
)
