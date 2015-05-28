# ShadyVenn
Creates Venn diagrams that encodes quantitative information as shades

# Description
Enables you to create Venn diagrams of 2, 3, or 4 sets. The bigger the overlap of a set, the deeper the color of the corresponding field. This aims to quickly assess quantitative informations with the Venn digram.
The script always exports the diagram as ".svg", from wich it can easily be further changed with any vector graphics editor,  e.g. Inkscape.

# Execution
ShadyVenn(input_lists, file_out)       #using the default settings

ShadyVenn(input_lists, file_out, color, venn_type, hide_values, fontSize )  # with specified settings

# Example input data
input_lists  <- list("SetA" = 1:180, "SetB" = 157:202, "SetC" = c(77:203,200:202), "SetD" = c(50:100, 10:20, 200:210))  # list of 2 to 4 lists of either strings or integers

color <- "crimson"		                     # base color of the plot

venn_type  <- c("2er","3er","4er")[3]      # If not provided, the skipt will use the maximal number depending on the input lists

hide_values <- FALSE                       # option to hide the concrete numbers within the fields; default is FALSE

file_out <- "4er_new"		                   # filename of the to be exported plot

fontSize <- 26                             # sets the font size if the overlap values
