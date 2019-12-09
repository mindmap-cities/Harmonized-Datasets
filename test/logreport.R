file.remove("Harmonized_dataset_new/test/test.log")
file.create("Harmonized_dataset_new/test/test.log")

con <- file("Harmonized_dataset_new/test/test.log")
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")

# This will echo all input and not truncate 150+ character lines...
source("/home/agurer/.Rprofile", echo=TRUE, max.deparse.length=10000)

# And look at the log...
file.show("Harmonized_dataset_new/test/test.log")
