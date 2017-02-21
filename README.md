# Damage to public health and economy by severe weather events

## Synopsis
Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

This project involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.

In this publication we are going to investigate:

1. Across the United States, which types of events are **most harmful with respect to population health**?
2. Across the United States, which types of events have the **greatest economic consequences**?

## Database

*Storm Data* is an official publication of the National Oceanic and Atmospheric Administration (NOAA) which documents: 

1. The occurrence of storms and other significant weather phenomena having sufficient intensity to cause loss of life, injuries, significant property damage, and/or disruption to commerce;
2. Rare, unusual, weather phenomena that generate media attention, such as snow flurries in South Florida or the San Diego coastal area; and 
3. Other significant meteorological events, such as record maximum or minimum temperatures or precipitation that occur in connection with another event. 

The database currently contains data from **January 1950 to April 2014**, as entered by NOAA's National Weather Service (NWS). Due to changes in the data collection and processing procedures over time, there are unique periods of record available depending on the event type. NCDC has performed data reformatting and standardization of event types but has not changed any data values for locations, fatalities, injuries, damage, narratives and any other event specific information. Please refer to the [Database Details](http://www.ncdc.noaa.gov/stormevents/details.jsp) page for more information. 

## Data processing

### Data loading

The data is available and can be freely downloaded at the following url [https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2)

```{r plot_preparation, collapse=TRUE}
library(ggplot2);
library(gridExtra);
```

```{r data_download, cache=TRUE}
config.filename <- "repdata%2Fdata%2FStormData.csv.bz2";
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
              config.filename, method="curl");
data.csv <- read.csv(bzfile(config.filename),sep=",",header=T);
```

### Event types cleanup

Let's look at given event types and if necessary do data processing and cleaning
```{r evtype_verification, size=2}
summary(data.csv$EVTYPE);

length(unique(data.csv$EVTYPE));
```
Some names are uppercase and some are not. For example we have event type *COSTAL FLOODING* and *costal flooding*. For further investigation we have to make all uppercase.

```{r evtype_reduction}
data.csv$EVTYPE <- toupper(data.csv$EVTYPE);
```
Compare if there are any changes in total number of unique event types
```{r evtype_comparison}
length(unique(data.csv$EVTYPE));
```

The number of unique event types has reduced which is important step when aggregating total sum by event type.
Also encode **EVTYPE** vector as a *factor*

```{r convert_to_factor}
data.csv$EVTYPE <- factor(data.csv$EVTYPE);
```

### Obtaining dollar amounts

Estimates should be rounded to three significant digits, followed by an alphabetical character signifying the magnitude of the number, i.e., `1.55B` for `$1,550,000,000`. Alphabetical characters used to signify magnitude include `K` for *thousands*, `M` for *millions*, and `B` for *billions*. If additional precision is available, it may be provided in the narrative part of the entry. First let's look at the magnitude levels.

```{r magnitude_levels}
# PROPDMGEXP
unique(data.csv$PROPDMGEXP);

# CROPDMGEXP
unique(data.csv$CROPDMGEXP);
```

As we can see some levels are uppercase and some are lowercase. We should make them all uppercase

```{r uppercase_magnitude_levels}
data.csv$PROPDMGEXP <- toupper(data.csv$PROPDMGEXP);
data.csv$CROPDMGEXP <- toupper(data.csv$CROPDMGEXP);
```

Some of the levels are missing or not defined in [NATIONAL WEATHER SERVICE INSTRUCTION](http://www.ncdc.noaa.gov/stormevents/pd01016005curr.pdf) Unknown levels will result in **zero** cost.

```{r calculate_costs, cache=TRUE}
magnify.estimates <- function(dmg, exp) {
    dmg <- round (as.numeric(dmg), digits=3);
    exp <- toupper(exp);
    if (exp == "K") {
        dmg <- dmg * 1e3; # Thousands
        return(dmg);
    }
    if (exp == "M") {
        dmg <- dmg * 1e6; # Millions
        return(dmg);
    }
    if (exp == "B") {
        dmg <- dmg * 1e9; # Billions
        return(dmg);
    }
    dmg <- 0;
    dmg;
}

data.csv$MAGPROPDMG <- mapply(magnify.estimates, 
                              data.csv$PROPDMG,
                              data.csv$PROPDMGEXP);

data.csv$MAGCROPDMG <- mapply(magnify.estimates, 
                              data.csv$CROPDMG,
                              data.csv$CROPDMGEXP);
```

#### Calculate greatest economic consequences

Finally we are going calculate total costs:

1. Sum all costs by event type
2. Sort by total cost
3. Select top 5 events

```{r prepare_data, cache=TRUE}
# Sum property and crop estimates
data.csv$TOTAL_DMG <- data.csv$MAGPROPDMG + data.csv$MAGCROPDMG;

# Choose only data we need for further analysis
# Totals
data.damages <- data.csv[,c("EVTYPE","MAGPROPDMG","MAGCROPDMG","TOTAL_DMG")];
data.damages.total_dmg <- aggregate (TOTAL_DMG ~ EVTYPE,data.damages,sum, na.rm=T);
data.damages.total_dmg <- data.damages.total_dmg[order(-data.damages.total_dmg$TOTAL_DMG),];
data.damages.top <- data.damages.total_dmg[1:5,];
data.damages.top$TOTAL_DMG <- data.damages.top$TOTAL_DMG/1e9;

# Crop totals
data.damages.crop_dmg  <- aggregate (MAGCROPDMG ~ EVTYPE,data.damages, sum, na.rm=T);
data.damages.crop_dmg  <- data.damages.crop_dmg[order(-data.damages.crop_dmg$MAGCROPDMG),];
data.damages.crop.top  <- data.damages.crop_dmg [1:5,];
data.damages.crop.top$TYPE <- factor("CROP");
names(data.damages.crop.top)[2]<-"DMG";

# Property Totals
data.damages.prop_dmg  <- aggregate (MAGPROPDMG ~ EVTYPE, data.damages, sum, na.rm=T);
data.damages.prop_dmg  <- data.damages.prop_dmg[order(-data.damages.prop_dmg$MAGPROPDMG),];
data.damages.prop.top  <- data.damages.prop_dmg[1:5,];
data.damages.prop.top$TYPE <- factor("PROP");
names(data.damages.prop.top)[2]<-"DMG";
```

I am going to produce merged dataset containing two type:

1. Property damage
2. Crop damage

This will help to understand better which event type affects separated costs for properties and crops.

```{r merge_damages}
# Merge property damages and crop damages datasets
data.damages.merged <- rbind(data.damages.crop.top,data.damages.prop.top);
data.damages.merged$DMG <- data.damages.merged$DMG/1e9;
```

#### Calculate most harmful with respect to population health

The same procedure (as we done with damaging costs) applies to **injuries** and **fatalities** data.

```{r prepare_harm_dataset, cache=TRUE}
data.harm <- data.csv[,c("EVTYPE","INJURIES","FATALITIES")];

data.harm.injuries <- aggregate (INJURIES ~ EVTYPE, data.harm,sum, na.rm=TRUE);
data.harm.injuries <- data.harm.injuries[order(-data.harm.injuries$INJURIES),];
data.harm.injuries.top <- data.harm.injuries[1:5,];
names(data.harm.injuries.top)[2] <- "TOTAL";
data.harm.injuries.top$TYPE <- factor("INJURIES");

data.harm.fatalities   <- aggregate (FATALITIES ~ EVTYPE, data.harm, sum, na.rm=TRUE);
data.harm.fatalities <- data.harm.fatalities[order(-data.harm.fatalities$FATALITIES),];
data.harm.fatalities.top <- data.harm.fatalities[1:5,];
names(data.harm.fatalities.top)[2] <- "TOTAL";
data.harm.fatalities.top$TYPE <- factor("FATALITIES");

data.harm.merged <- rbind(data.harm.injuries.top, data.harm.fatalities.top);
```

## Results
### Damage to economy
As we can see from *Figure 1* the most damaging event to economy is **FLOOD**. However majority of the damage during floods are done to the properties. **DROUGHT** cause most damage to **crops**.

#### Figure 1
```{r damage_figure}
qp1 <- qplot(x=EVTYPE, y=DMG, fill=TYPE,
            data=data.damages.merged, geom="bar", stat="identity",
            position="dodge") + labs(x="Event",  y="Billion Dollars", title="Damage to Economy")+
            coord_flip()

qp2 <- qplot(x=EVTYPE, y=TOTAL_DMG, fill=EVTYPE,
            data=data.damages.top, geom="bar", stat="identity",
            position="dodge") + labs(x="Event",  y="Billion Dollars")+ 
            coord_flip();

# Group together two plots into one
grid.arrange(qp1,qp2);
```

### Damage to public health
*Figure 2* shows that most harmful event is **tornado**.

#### Figure 2
```{r harmful_figure}
g <- ggplot(data.harm.merged,aes(factor(EVTYPE),TOTAL,fill=TYPE)) +
    geom_bar(stat="identity") +
    facet_grid(.~TYPE) +
    labs(x="Event type",  y="People") + 
    labs(title="Damage to public health") + coord_flip();

qp <- qplot(x=EVTYPE, y=TOTAL, fill=TYPE,
            data=data.harm.merged, geom="bar", stat="identity",
            position="dodge") + labs(x="Event",  y="People") + coord_flip();

# Group together two plots into one
grid.arrange(g,qp);
```

## Further research and improvements

Different types were introduced at the different years. From 1954 more event types were introduced. 

1. Tornado: From 1950 through 1954, only tornado events were recorded.
2. *Tornado, Thunderstorm Wind and Hail*: From 1955 through 1992, only tornado, thunderstorm wind and hail events were keyed from the paper publications into digital data. From 1993 to 1995, only tornado, thunderstorm wind and hail events have been extracted from the [Unformatted Text Files](http://www.ncdc.noaa.gov/stormevents/details.jsp?type=collection).
3. All Event Types (48 from Directive 10-1605): From 1996 to present, 48 event types are recorded as defined in [NWS Directive 10-1605](http://www.ncdc.noaa.gov/stormevents/pd01016005curr.pdf). 

It suggests that some of the types can be grouped together. Grouping similar event types can give broader understanding about larger events. Other options would be to split and work with different periods.

Also as we noticed data presents high variability, which gives us good reason to use log scales.


