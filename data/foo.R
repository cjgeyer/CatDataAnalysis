
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

