source(paste0(getwd(),"/","ShadyVenn.R"))

#example input data
input_lists <- list("SetA" = 1:180, "SetB" = 157:202, "SetC" = c(77:203,200:202), "SetD" = c(50:100, 10:20, 200:210))  # list of 2 to 4 lists of either strings or integers
color <- "purple"		                   #base color of the plot
type  <- c("2er","3er","4er")[2]       #currently, only 2 to 4 sets can be plotted. If not provided, the skipt will use the maximal number depending on the input lists
hide_values <- FALSE                   #option to hide the concrete numbers within the fields; default is FALSE
file_out <- "test_Shady"		             #filename of the to be exported plot


#plot and export it
ShadyVenn(input_lists, file_out)       #using the default settings
ShadyVenn(input, file_out, color, type, hide_values)  # with specified settings

#support function: gives you all the colors available
ShadyVenn.get_colors()
