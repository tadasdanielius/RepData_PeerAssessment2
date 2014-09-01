#install.packages("stringr", dependencies=TRUE)
#require(stringr)
library(gridExtra);


config.filename <- "repdata%2Fdata%2FStormData.csv.bz2";
# Downloading the data
#   data loading
if (!file.exists(config.filename)) {
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",config.filename);
}
if (!exists("data.csv") || is.null(data.csv)) {
    data.csv <- read.csv(bzfile(config.filename),sep=",",header=T);
}

# Data processing

# Let's look at given event types and if necessary do data processing and cleaning
unique(data.csv$EVTYPE);
length(unique(data.csv$EVTYPE));
# Some names are uppercase and some not. For further investigation I will make all 
# types uppercase and compare if there are any changes in total number of unique 
# event types

data.csv$EVTYPE <- toupper(data.csv$EVTYPE);
length(unique(data.csv$EVTYPE));

# The number of unique event types are reduced which is important step when 
# aggregating total sum by event type.

data.csv$EVTYPE <- factor(data.csv$EVTYPE);


head(data.csv[,25:28]);

# PROPDMGEXP
unique(data.csv$PROPDMGEXP);

# CROPDMGEXP
unique(data.csv$CROPDMGEXP);

# Uperrcase
data.csv$PROPDMGEXP <- toupper(data.csv$PROPDMGEXP);
data.csv$CROPDMGEXP <- toupper(data.csv$CROPDMGEXP);


# PROPDMGEXP
unique(data.csv$PROPDMGEXP);

# CROPDMGEXP
unique(data.csv$CROPDMGEXP);


magnify.estimates <- function(dmg, exp) {
    dmg <- round (as.numeric(dmg), digits=3);
    exp <- toupper(exp);
    if (exp == "K") {
        dmg <- dmg * 1e3;
        return(dmg);
    }
    if (exp == "M") {
        dmg <- dmg * 1e6;
        return(dmg);
    }
    if (exp == "B") {
        dmg <- dmg * 1e9;
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

data.csv$TOTAL_DMG <- data.csv$MAGPROPDMG + data.csv$MAGCROPDMG;

data.damages <- data.csv[,c("EVTYPE","MAGPROPDMG","MAGCROPDMG","TOTAL_DMG")];
data.damages.total_dmg <- aggregate (TOTAL_DMG ~ EVTYPE,data.damages,sum, na.rm=T);
data.damages.total_dmg <- data.damages.total_dmg[order(-data.damages.total_dmg$TOTAL_DMG),];
data.damages.top <- data.damages.total_dmg[1:5,];
data.damages.top$TOTAL_DMG <- data.damages.top$TOTAL_DMG/1e9;

data.damages.crop_dmg  <- aggregate (MAGCROPDMG ~ EVTYPE,data.damages, sum, na.rm=T);
data.damages.crop_dmg  <- data.damages.crop_dmg[order(-data.damages.crop_dmg$MAGCROPDMG),];
data.damages.crop.top  <- data.damages.crop_dmg [1:5,];
data.damages.crop.top$TYPE <- factor("CROP");
names(data.damages.crop.top)[2]<-"DMG";


data.damages.prop_dmg  <- aggregate (MAGPROPDMG ~ EVTYPE, data.damages, sum, na.rm=T);
data.damages.prop_dmg  <- data.damages.prop_dmg[order(-data.damages.prop_dmg$MAGPROPDMG),];
data.damages.prop.top  <- data.damages.prop_dmg[1:5,];
data.damages.prop.top$TYPE <- factor("PROP");
names(data.damages.prop.top)[2]<-"DMG";

data.damages.merged <- rbind(data.damages.crop.top,data.damages.prop.top);
data.damages.merged$DMG <- data.damages.merged$DMG/1e9;

d<-data.damages.merged;
d_sum<-data.damages.top;

qp1 <- qplot(x=EVTYPE, y=DMG, fill=TYPE,
            data=data.damages.merged, geom="bar", stat="identity",
            position="dodge") + labs(x="Event type",  y="Damage");


qp2 <- qplot(x=EVTYPE, y=TOTAL_DMG, fill=EVTYPE,
            data=data.damages.top, geom="bar", stat="identity",
            position="dodge") + labs(x="Event type",  y="Total Damage");


grid.arrange(qp2,qp1);


# Find out how many different event types our dataset contains
length(unique(data.csv$EVTTYPE));

#   Selecting subset of the data

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

g <- ggplot(data.harm.merged,aes(factor(EVTYPE),TOTAL,fill=TYPE)) +
    geom_bar(stat="identity") +
    facet_grid(.~TYPE) +
    labs(x="Event type",  y="Harm") + 
    labs(title="Harmful events")


qp <- qplot(x=EVTYPE, y=TOTAL, fill=TYPE,
            data=data.harm.merged, geom="bar", stat="identity",
            position="dodge") + labs(x="Event type",  y="Total");


grid.arrange(g,qp);
