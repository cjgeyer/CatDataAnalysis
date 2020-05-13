
library("xml2")

u <- "http://www.stat.ufl.edu/~aa/cda/data.html"
cmd <- paste("wget --no-clobber",
    "--user-agent='Mozilla/5.0 (X11; Linux i586; rv:31.0)",
    "Gecko/20100101 Firefox/73.0'", u)
system(cmd)
foo <- read_html(file("data.html"))

bar <- xml_find_all(foo, "//b")
headers <- xml_text(bar)

bar <- xml_find_all(foo, "//pre")
data.blocks <- xml_text(bar)

bar <- xml_find_all(foo, "//p")
para.blocks <- xml_text(bar)

length(headers)
length(data.blocks)

# One Bogus Header
headers <- headers[seq(along = data.blocks)]
# Remove newlines
headers <- gsub("\\n", "", headers)

# Para blocks really fubar.
# But some are headers.
para.blocks <- gsub("\\n", "", para.blocks)
para.blocks <- para.blocks[para.blocks != ""]
innies <- match(para.blocks, headers)

# only stuff in paragraph mode; goes with item 1
para.blocks[is.na(innies)]

# now need to fix up data blocks
data.blocks <- as.list(data.blocks)
data.blocks <- lapply(data.blocks, strsplit, split = "\\n")
data.blocks <- lapply(data.blocks, unlist)
all(sapply(data.blocks, function(x) x[1]) == "")
data.blocks <- lapply(data.blocks, function(x) x[-1])
# now look at first lines, which are a mess still
sapply(data.blocks, function(x) x[1])
# everything after " * " is comment, get comment
comments <- sapply(data.blocks, function(x) {
    foo <- unlist(strsplit(x[1], split = " * ", fixed = TRUE))
    if (length(foo) == 2) foo[2] else ""
})
# now strip comments
data.blocks <- lapply(data.blocks, function(x) {
    foo <- unlist(strsplit(x[1], split = " * ", fixed = TRUE))
    x[1] <- foo[1]
    return(x)
})
all(sapply(data.blocks, class) == "character")
lapply(data.blocks, function(x) cbind(x[1:4]))
# item 3 is foobar: no variable names, must fix by hand
# items 6 and 23 have line "datalines;", remove programmatically
# items 8, 9, 12, 15, 16 are tables; this is the HARD PART
#     for now just recognize and do not mess up
# item 23 has semicolon at end of variable names, remove programmatically
# item 29 and perhaps others have invalid R symbols for variable names
#     fix by running through R function make.names
# item 30 is foobar: variable names on two lines followed by blank line,
#     must fix by hand
# some other items (not shown here) have extra blank lines that must be
#     removed programmatically

# item 3
headers[3]
head(data.blocks[[3]])
data.blocks[[3]] <- c("game made attempts", data.blocks[[3]])

# "datalines;"
data.blocks <- lapply(data.blocks, function(x)
    grep("^datalines;", x, invert = TRUE, value = TRUE))

# semicolon
lapply(data.blocks, function(x) grep(";", x, value = TRUE))
data.blocks <- lapply(data.blocks, function(x) sub(";", "", x))

# item 30
headers[30]
cbind(data.blocks[[30]][1:4])
data.blocks[[30]] <-
    c("county perot_vote total_vote_1996 buchanan_vote total_vote_2000",
    data.blocks[[30]][-c(1:3)])

# now kill two birds with one stone (1) identify valid data frames
# and, consequently tables (everything else) and (2) strip blank lines
# to make valid data frames
count <- function(x) {
    foo <- strsplit(x, split = "  *")
    bar <- lapply(foo, function(x) x[x != ""])
    sapply(bar, length)
}
item_counts <- lapply(data.blocks, count)
lapply(item_counts, unique)
# what's up with items 15 and 23?
cbind(data.blocks[[15]])
cbind(data.blocks[[23]])
# 15 is actually a table
# the word "input" on line 1 of item 23 is bogus
data.blocks[[23]] <- sub("input", "", data.blocks[[23]])

item_counts <- lapply(data.blocks, count)
lapply(item_counts, unique)
# items with only one nonzero number in above are tables
item_counts_nonzero <- lapply(item_counts, function(x) x[x != 0])
is.valid.data.frame <- sapply(lapply(item_counts_nonzero, unique), length) == 1
is.unchecked.table <- (! is.valid.data.frame)
# data.blocks[is.valid.data.frame]
# data.blocks[is.unchecked.table]
data.blocks <- mapply(
    function(i, x) if (i) grep("^ *$", x, invert = TRUE, value = TRUE) else x,
    is.valid.data.frame, data.blocks, SIMPLIFY = FALSE, USE.NAMES = FALSE)

# check no blank lines
item_counts <- lapply(data.blocks, count)
item_counts_unique <- lapply(item_counts, unique)
item_counts_unique_lengths <- sapply(item_counts_unique, length)
all(item_counts_unique_lengths[is.valid.data.frame] == 1)
item_counts_unique_lengths[is.unchecked.table]

# still any problems?
# lapply(data.blocks, cbind)
# there are tabs, get rid of them
data.blocks <- lapply(data.blocks, function(x) sub("\\t", "", x))
# detect and show non-ASCII
data.blocks.fubar <- lapply(data.blocks, tools::showNonASCII)
all(data.blocks.fubar == character(0))
# OK.  Unix command less was showing something funny, but appears bogus

# OK!  That was the EASY PART, now for the HARD PART
data.blocks.tables <- data.blocks[is.unchecked.table]
names(data.blocks.tables) <- make.names(headers[is.unchecked.table])
data.blocks.tables
# in data set for Exercises 6.3 and 9.13 it appears
# all counts are >= 4
# all category labels are <= 3
# so can distinguish
#
# in data set for Exercise 6.7
# all counts are numbers (of course)
# all category labels are non-numeric
#
# in data set for Exercise 6.28
# all counts are numbers (of course)
# all category labels are non-numeric
#
# in data set for Exercise 8.18
# all counts are numbers (of course)
# all category labels are non-numeric except for those in first line (oof!)
#
# in data for Exercise 8.28
# all counts are numbers (of course)
# all category labels are non-numeric
get_numbers <- function(x, min.count = 0, skip.lines = 0) {
    if (skip.lines > 0)
        x <- x[- seq(1, skip.lines)]
    foo <- strsplit(x, split = " +")
    bar <- grep("^[[:digit:]]+$", unlist(foo), value = TRUE)
    baz <- as.numeric(bar)
    return(baz[baz >= min.count])
}
min.count <- c(4, 0, 0, 0, 0)
skip.lines <- c(0, 0, 0, 1, 0)
data.blocks.tables.numbers <- mapply(get_numbers, data.blocks.tables,
    min.count, skip.lines, SIMPLIFY = FALSE, USE.NAMES = FALSE)
# OK!  We have the numbers, in byrow = TRUE order, so be careful
# Now we need the factor names and levels, which we supply as a list of lists
data.blocks.tables.factors <- list(
    list(Birth_control = 1:2, Religious_Attendence = 1:2,
        Premarital_Sex = 1:2, Political_Views = 1:3),
    # Gender not in file, taken from book
    list(Gender = c("M", "F"), Age = c("<35", "35-44", ">44"),
        Race = c("White", "other"),
        Satisfaction = c("Satisfied", "Not satisfied"),
        Region = c("Northeast", "Mid-Atlantic", "Southern",
            "Midwest", "Northwest", "Southwest", "Pacific")),
    list(Occupational_aspirations = c("High", "Low"),
        Socioeconomic_status = c("High", "Low"), IQ = c("High", "Low"),
        Residence = c("Rural", "Small urban", "Large urban"),
        Gender = c("Male", "Female")),
    # Dumping_severity not in file, taken from book
    list(Dumping_severity = c("N", "S", "M"), Hospital = 1:4,
        Operation = LETTERS[1:4]),
    list(Satisfaction = c("Low", "Medium", "High"), Contact = c("Low", "High"),
        Influence = c("Low", "Medium", "High"),
        Housing = c("Tower blocks", "Apartments",
            "Atrium houses", "Terraced houses")))
data.blocks.tables.factors

# Now to make the data frames
to_data_frame <- function(n, f) {
    # n is counts vector, f is list of factor names and levels,
    # first goes fastest
    l <- sapply(f, length)
    stopifnot(length(n) == prod(l))
    for (i in seq(along = f)) {
       each <- prod(l[seq(along = l) < i])
       times <- prod(l[seq(along = l) > i])
       f[[i]] <- rep(f[[i]], each = each)
       f[[i]] <- rep(f[[i]], times = times)
    }
    f <- lapply(f, as.factor)
    f$counts <- n
    as.data.frame(f)
}
data.blocks.tables.data.frames <- mapply(to_data_frame,
    data.blocks.tables.numbers, data.blocks.tables.factors,
    SIMPLIFY = FALSE, USE.NAMES = FALSE)
data.blocks.tables.data.frames

# make the tables

xtabs(counts ~ ., data.blocks.tables.data.frames[[1]])
xtabs(counts ~ ., data.blocks.tables.data.frames[[2]])
xtabs(counts ~ ., data.blocks.tables.data.frames[[3]])
xtabs(counts ~ ., data.blocks.tables.data.frames[[4]])
xtabs(counts ~ ., data.blocks.tables.data.frames[[5]])

# now get data frames for the rest

data.blocks.data.frame <- list()
foo <- data.blocks[is.valid.data.frame]
for (i in seq(along = foo)) {
    write(foo[[i]], file = "foompter.txt")
    data.blocks.data.frame[[i]] <- read.table("foompter.txt", header = TRUE)
}
names(data.blocks.data.frame) <- headers[is.valid.data.frame]
print(lapply(data.blocks.data.frame, head), width = 132)

# not want to compare with old data sets (2016)

d <- "/home/geyer/ClassNotes/5421/Data"
cmd <- paste("find", d, "-type f",
    "\\( -name 'exercise-*.txt' -o -name 'table-*.txt' \\)")
files <- system(cmd, intern = TRUE)
files

files.base <- basename(files)
sort(files.base)

files.key <- sub("\\.txt$", "", files.base)
files.key <- sub("-", "s* ", files.key)
files.key
files.idx <- sapply(files.key,
    function(x) grep(x, headers, ignore.case = TRUE))
files.idx
headers

# Arrrrrgh!
# Table 10.9 is for Exercise 10.4.  I guess I added this.  It is not
#     in the current HTML data
# Table 4.11 is for Exercise 4.13.  Have to match latter.
# Table 8.18 is for Exercise 8.8.  I guess I added this.  It is not
#     in the current HTML data

files.key[files.key == "tables* 4.11"] <- "exercises* 4.13"
files.idx <- lapply(files.key,
    function(x) grep(x, headers, ignore.case = TRUE))
files.idx <- sapply(files.idx, function (x) if (length(x) == 0) NA else x)
files.idx <- files.idx[! is.na(files.idx)]
files.idx
setdiff(headers, headers[files.idx])

# I guess there were a bunch we skipped in 2016
# and 2 we added
# so much for x-check with that
#
# skip x-check for now

# Extract data set names from headers

headers.table <- sub("^.*(Table [0-9.]*).*$", "\\1", headers)
headers.table
! any(grepl("Table.*Table", headers))
# Got all the tables
headers.exercise <- sub("^.*(Exercise [0-9.]*).*$", "\\1", headers)
! any(grepl("Exercise.*Exercise", headers))
headers.exercise
# Got all the exercises, not!
idouble <- grep("Exercises", headers)
headers[idouble]
# also there is one left
headers.section <- sub("^.*(Section [0-9.]*).*$", "\\1", headers)
headers.section

headers.is.table <-
    sapply(strsplit(headers.table, split = "  *"), length) == 2
headers.table[headers.is.table]
headers.is.exercise <-
    sapply(strsplit(headers.exercise, split = "  *"), length) == 2
headers.exercise[headers.is.exercise]
headers.is.section <-
    sapply(strsplit(headers.section, split = "  *"), length) == 2
headers.section[headers.is.section]
headers.is.other <- ! (headers.is.table | headers.is.exercise |
    headers.is.section)
headers[headers.is.other]
headers[headers.is.table & headers.is.exercise]

# OK!  Ready to write data files and help files

unlink("../package/CDA/data", recursive = TRUE, expand = FALSE)
unlink("../package/CDA/man", recursive = TRUE, expand = FALSE)
dir.create("../package/CDA/data", recursive = TRUE)
dir.create("../package/CDA/man", recursive = TRUE)

datasetnames <- rep(NA_character_, length(headers))
datasetnames[headers.is.section] <- headers.section[headers.is.section]
datasetnames[headers.is.exercise] <- headers.exercise[headers.is.exercise]
datasetnames[headers.is.table] <- headers.table[headers.is.table]
foo <- sub("^.*Exercises ([0-9.]*) and ([0-9.]*)", "\\1 \\2", headers)
foo[headers.is.other]
foo <- unlist(strsplit(foo[headers.is.other], " "))
foo
datasetnames[headers.is.other] <- paste("Exercise", foo[1])
datasetaliases <- rep(NA_character_, length(headers))
datasetaliases[headers.is.other] <- paste("Exercise", foo[2])
datasetaliases[headers.is.table & headers.is.exercise] <-
    headers.exercise[headers.is.table & headers.is.exercise]
datasetnames <- tolower(datasetnames)
datasetaliases <- tolower(datasetaliases)
datasetnames <- sub(" ", "_", datasetnames)
datasetaliases <- sub(" ", "_", datasetaliases)
datasetnames
datasetaliases
dataset.is.duplicated <- duplicated(datasetnames)
dataset.is.duplicated
# for now just let duplicate overwrite the other

datasets <- list()
datasets[is.valid.data.frame] <- data.blocks.data.frame
datasets[! is.valid.data.frame] <- data.blocks.tables.data.frames
names(datasets) <- datasetnames
# datasets

for (i in seq(along = datasets)) {
    foo <- names(datasets)[i]
    assign(foo, datasets[[i]])
    save(list = foo,
        file = file.path("../package/CDA/data", paste0(foo, ".rda")))
}

# Woot!  Now write man pages.

setwd("../package/CDA/man")

for (i in seq(along = headers)) {
    myname <- datasetnames[i]
    myalias <- datasetaliases[i]
    filename <- paste0(myname, ".Rd")
    cat("i =", i, ", myname =", myname, ", myalias =", myalias,
        ", file =", filename, "\n")
    mycat <- function(..., sep = "", append = TRUE)
        cat(..., file = filename, sep = sep, append = append)
    mycat("\\name{", myname, "}\n", append = FALSE)
    mycat("\\docType{data}\n")
    mycat("\\alias{", myname, "}\n")
    if (! is.na(myalias))
        mycat("\\alias{", myalias, "}\n")
    mytitle <- headers[i]
    mytitle <- sub("[0-9]*\\. ", "", mytitle)
    mytitle <- trimws(mytitle)
    mydescription <- paste0(mytitle, ".")
    mytitle <- tools::toTitleCase(mytitle)
    mycat("\\title{", mytitle, "}\n")
    mycat("\\description{", mydescription, "}\n")
    mycat("\\usage{data(", myname, ")}\n")
    mycat("\\format{A data frame containing", nrow(datasets[[i]]),
        "observations (rows)\n", sep = " ")
    varnames <- names(datasets[[i]])
    mycat("  and the following columns:\n")
    mycat("  \\describe{\n")
    for (vn in varnames)
        mycat("  \\item{", vn, "}{}\n")
    mycat("  }\n")
    mycat("}\n")
    mycat("\\source{\\url{http://www.stat.ufl.edu/~aa/cda/data.html}}\n")
    mycat("\\references{\n")
    mycat("  Agresti, A. (2013)\n")
    mycat("  \\emph{Categorical Data Analysis}, Third Edition.\n")
    mycat("  Hoboken, NJ: John Wiley \\& Sons.\n")
    mycat("  ISBN: 978-0-470-46363-5.\n")
    mycat("}\n")
    if (! is.valid.data.frame[i]) {
        mycat("\\examples{\n")
        mycat("# in the source was a table rather than a data frame\n")
        mycat("# to convert to a table do\n")
        mycat("data(", myname, ")\n")
        mycat(myname, "_as_table <- xtabs(counts ~ ., data = ", myname, ")\n")
        mycat("\\dontrun{", myname, "_as_table}\n")
        mycat("}\n")
    }
    cat("\\keyword{datasets}\n", file = filename, append = TRUE)
}

headers[! is.valid.data.frame]
