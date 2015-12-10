
# Load required libraries
library(plyr)
library(splitstackshape)
library(reshape)
suppressPackageStartupMessages(library(plyr))
library(stringr)

# Set working directory to folder containing folders named "input" and "output.
# Input folder should contain full records and citations downloaded from 
# Web of knowledge in tab-delimited (UTF-16) format. Working directory should 
# contain the list of fieldtags for naming the variables (fieldtags.csv).
###############################################################################

# Load variable names
fieldtags <- read.csv("fieldtags.csv", header = T, 
                      sep = ";")

# List files in input folder
filelist <- list.files("input", full.names = T)

patents <- ldply(filelist, read.delim2, fileEncoding="UTF-16", 
                 quote="", row.names=NULL)

# Fix column names
data.names <- names(patents)[2:length(names(patents))]
patents <- patents[, 1:(ncol(patents) - 1)]
names(patents) <- data.names

# Remove duplicates
patents <- patents[!duplicated(patents), ]

# Add id variable
patents$id <- c(1:nrow(patents))


###############################################################################

# Cleaning data

# Fix variable names
tags <- names(patents)
fields <- as.character(fieldtags$field[match(tags, fieldtags$tag)])
fields[is.na(fields)] <- tags[is.na(fields)]     # Throws warnings but seems to be working
fields <- gsub(" ", "", fields)
fields <- gsub("\\(", "", fields)
fields <- gsub("\\)", "", fields)
names(patents) <- fields

# Fix variable types
patents$PatentDetails <- as.character(patents$PatentDetails)
patents$InternationalPatentClassification <- as.character(patents$InternationalPatentClassification)
patents$Inventors <- as.character(patents$Inventors)
patents$Assignees <- as.character(patents$Assignees)

# Helper function to remove leading and trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# Extract date and year
patents$Date <- unlist(lapply(patents$PatentDetails, 
                              function(x) str_extract(x, "[0-9]{2} [A-Za-z]{3} [0-9]{4}")))
patents$Date <- strptime(patents$Date, "%d %b %Y")
patents$Year <- as.integer(format(patents$Date, "%Y"))

# Extract International Patent Classification (IPC)
patents$Class <- substr(patents$InternationalPatentClassification, 1, 3)
patents$Class <- as.factor(patents$Class)

# Number of inventors
count_inventors <- function(x) {
  inventorList <- unlist(strsplit(x, ";"))
  return(length(inventorList))
}

patents$InventorCount <- unlist(lapply(patents$Inventors, count_inventors))
patents$AssigneeCount <- unlist(lapply(patents$Assignees, count_inventors))


# Save patents as a single csv-file literature.csv.
write.table(patents, "output/patents.csv", 
            sep = ";", row.names = F, qmethod = "double")

###############################################################################

# Create a new data frame, where each inventor is in a separate row

# Subset data
patentsByInventor = subset(patents, 
                            select = c("Inventors", "id"))

# Create data frame: Inventors split by ";", each name on a new row, 
# id copied to new rows
patentsByInventor <- cSplit(patentsByInventor, 
                                            splitCols = "Inventors", 
                                            sep = ";", direction = "long")
# Merge the rest of the data by id
patentsByInventor <- merge(patentsByInventor, 
                            subset(patents, select = -c(Inventors, Date)), 
                            by = "id", all.x = TRUE)
# Save file
write.table(patentsByInventor, "output/patents_by_inventor.csv", 
            sep = ";", row.names = F, qmethod = "double")

###############################################################################

# Create a new data frame, where each class is in a separate row

# Subset data
patentsByIPC = subset(patents, 
                           select = c("InternationalPatentClassification", "id"))

# Create data frame: Inventors split by ";", each name on a new row, 
# id copied to new rows
patentsByIPC <- cSplit(patentsByIPC, 
                            splitCols = "InternationalPatentClassification", 
                            sep = ";", direction = "long")
# Merge the rest of the data by id
patentsByIPC <- merge(patentsByIPC, 
                           subset(patents, select = -c(InternationalPatentClassification, 
                                                       Date)), 
                           by = "id", all.x = TRUE)

patentsByIPC$Class <- substr(patentsByIPC$InternationalPatentClassification, 1, 3)
patentsByIPC$Class <- as.factor(patentsByIPC$Class)

# Save file
write.table(patentsByIPC, "output/patents_by_ipc.csv", 
            sep = ";", row.names = F, qmethod = "double")

###############################################################################

# Create a new data frame, where each assignee is in a separate row

# Subset data
patentsByAssignee = subset(patents, 
                           select = c("Assignees", "id"))

# Create data frame: Inventors split by ";", each name on a new row, 
# id copied to new rows
patentsByAssignee <- cSplit(patentsByAssignee, 
                            splitCols = "Assignees", 
                            sep = ";", direction = "long")
# Merge the rest of the data by id
patentsByAssignee <- merge(patentsByAssignee, 
                           subset(patents, select = -c(Assignees, Date)), 
                           by = "id", all.x = TRUE)
patentsByAssignee$Companies <- abbreviate(patentsByAssignee$Assignees, 12)
# Save file
write.table(patentsByAssignee, "output/patents_by_assignee.csv", 
            sep = ";", row.names = F, qmethod = "double")

###############################################################################
# Create node and edge tables for inventor network

# Extract inventor names, remove whitespace, and turn into a data frame
inventors <- unlist(strsplit(patents$Inventors, ";"))
inventors <- trim(inventors)
inventors <- as.data.frame(table(inventors))
names(inventors)[1] <- "Id"
inventors$Label <- inventors$Id

# Save author node table
write.table(inventors, "output/inventor_nodes.csv", 
            sep = ';', quote = F, row.names = F)

# Helper functions for extracting edges

# Paste two nodes together
Collapser <- function(node){
    x <- paste(node, collapse = ';')
}

# Create node from a string containing author names of a paper
CreateNodes <- function(x){
    nodes <- unlist(strsplit(x, ';'))
    if(length(nodes) > 1){
        nodes <- combn(nodes, 2, simplify = F)
        nodes <- lapply(nodes, Collapser)
    } else{nodes <- NA}
    return(nodes)
}

# Count the length of nodes created from each row
inventors <- lapply(patents$Inventors, CreateNodes)
nodelengths <- sapply(inventors, length)

# Create nodes and put into a data frame
nodes <- unlist(lapply(patents$Inventors, CreateNodes))
nodes <- as.data.frame(nodes)
nodes <- as.data.frame(str_split_fixed(nodes$nodes, ";", 2))

# Fix column names
names(nodes) <- c("Source", "Target")

# Create id and edge type columns
nodes$id <- rep(patents$id, nodelengths)
nodes$Type <- rep("Undirected", nrow(nodes))

# Remove NAs from Source column
nodes <- nodes[!is.na(nodes$Source), ]

# Remove leading and trailing whitespace from Sources and Targets
nodes$Source <- trim(nodes$Source)
nodes$Target <- trim(nodes$Target)

# Merge with literature
nodes <- merge(nodes, subset(patents, select = -c(Inventors)), 
               by.x = "id", by.y = "id")

# Subset data. Use this to select columns to include in network data
nodes <- subset(nodes, 
                select = c("Source", "Target", "Type", "id",          # Don't change
                           "Year", "DocumentTitle", "PatentNumbers"   # Change
                           ))                                  

# Save author edge table
write.table(nodes, "output/inventor_edges.csv", 
            sep = ';', row.names = F)

###############################################################################
# Create node and edge tables for assignee network

# Extract inventor names, remove whitespace, and turn into a data frame
assignees <- unlist(strsplit(patents$Assignees, ";"))
assignees <- trim(assignees)
assignees <- as.data.frame(table(assignees))
names(assignees)[1] <- "Id"
assignees$Label <- assignees$Id

# Save author node table
write.table(assignees, "output/assignee_nodes.csv", 
            sep = ';', quote = F, row.names = F)

# Count the length of nodes created from each row
assignees <- lapply(patents$Assignees, CreateNodes)
nodelengths <- sapply(assignees, length)

# Create nodes and put into a data frame
nodes <- unlist(lapply(patents$Assignees, CreateNodes))
nodes <- as.data.frame(nodes)
nodes <- as.data.frame(str_split_fixed(nodes$nodes, ";", 2))

# Fix column names
names(nodes) <- c("Source", "Target")

# Create id and edge type columns
nodes$id <- rep(patents$id, nodelengths)
nodes$Type <- rep("Undirected", nrow(nodes))

# Remove NAs from Source column
nodes <- nodes[!is.na(nodes$Source), ]

# Remove leading and trailing whitespace from Sources and Targets
nodes$Source <- trim(nodes$Source)
nodes$Target <- trim(nodes$Target)

# Merge with literature
nodes <- merge(nodes, subset(patents, select = -c(Assignees)), 
               by.x = "id", by.y = "id")

# Subset data. Use this to select columns to include in network data
nodes <- subset(nodes, 
                select = c("Source", "Target", "Type", "id",          # Don't change
                           "Year", "DocumentTitle", "PatentNumbers"   # Change
                ))                                  

# Save author edge table
write.table(nodes, "output/assignee_edges.csv", 
            sep = ';', row.names = F)
