
To install, do (in R)

    install.packages("remotes")
    library(remotes)
    install_github("cjgeyer/CatDataAnalysis", subdir = "package/CatDataAnalysis")

Of course, you don't need the `install.package` command if you have already
installed CRAN package `remotes`.

After you have done the installation

    library("CatDataAnalysis")
    data(exercise_13.17)
    head(exercise_13.17)

shows that it worked (if you see the first few rows of the data when you
execute the `head` command).
