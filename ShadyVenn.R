ShadyVenn <-function(input, file_out, color = "red", type = "default", hide_values = FALSE){
	
	color_brewer <- list("neonyellow" = "#ccff00", "red" = "#ff0000", "purple" = "#6500ff", "blue" = "#000080", "lightblue" = "#007cd2", "grassy-green" = "#7cd200", "crimson" = "#ba1a1a", "lilac" = "#d5bee0", "babyblue" = "#aed5fc", "grey" = "#808080", "green" = "#008000", "teal" = "#00ac93","black" = "#000000", "brown" = "#561a05", "aquamarine" = "#7FFFD4", "azure" = "#007FFF", "greenyellow" = "#ADFF2F", "lemon" = "#FFF700", "mint" = "#98FF98", "neongreen" = "#39FF14", "orange" = "#FF6700", "yellow" = "#FFFF00")
	#just for ShadyVenn.get_colors(); breaks afterwards.
	if (type =="just_color") {
		return(color_brewer)
	}
	
	
	#formate input
	set_names <- gsub("[[:space:]]", " ", names(input))
	input_lists <- list( "A" =	unlist_F(input[1]), "B" =	unlist_F(input[2]),"C" =	unlist_F(input[3]),"D" =	unlist_F(input[4]))
	if (type == "default") { type <- paste0(length(input),"er") }
	#if (substring(file_out,2,2) != ":") {wdir <- getwd()}
	
	
	
	# calculate overlap of the lists
	sets <- data.frame(
		"A"  =  length(setdiff(input_lists$A, union(union(input_lists$B,input_lists$C),input_lists$D))),
		"B"  =  length(setdiff(input_lists$B, union(union(input_lists$A,input_lists$C),input_lists$D))),
		"C"  =  length(setdiff(input_lists$C, union(union(input_lists$B,input_lists$A),input_lists$D))),
		"D"  =  length(setdiff(input_lists$D, union(union(input_lists$B,input_lists$C),input_lists$A))),
		"AB" =	length(intersect(input_lists$A, input_lists$B)),
		"AC" =	length(intersect(input_lists$A, input_lists$C)),
		"AD" =	length(intersect(input_lists$A, input_lists$D)),
		"BC" =	length(intersect(input_lists$B, input_lists$C)),
		"BD" =	length(intersect(input_lists$B, input_lists$D)),
		"CD" =	length(intersect(input_lists$C, input_lists$D)),
		"ABC" =	length(intersect(intersect(input_lists$A, input_lists$B), input_lists$C )),
		"ABD" =	length(intersect(intersect(input_lists$A, input_lists$B), input_lists$D )),
		"ACD" =	length(intersect(intersect(input_lists$A, input_lists$C), input_lists$D )),
		"BCD" =	length(intersect(intersect(input_lists$B, input_lists$C), input_lists$D )),

		"ABCD" =	length(intersect(intersect(input_lists$A, input_lists$B),intersect(input_lists$C, input_lists$D)))
	)
	sets <- rbind(sets, "ratio" = sets/max(sets))
	row.names(sets) <- c("counts", "ratios")
	#print(sets)
	
	
	#change base_svg to the values supplied in the input data
	if (type == "4er") {
		svg_text <- Venn4er_base()
	} else if (type == "3er") {
		svg_text <- Venn3er_base()
	} else if (type == "2er") {
		svg_text <- Venn2er_base()
	} else {print("Error: could not identify VennType")}
	
	#change color
	color_hex <- color_brewer[[color]]
	svg_text <- gsub("fill:#ff0000",     paste0("fill:",color_hex)  , svg_text)
	
	#change set names
	svg_text <- gsub("name_A<",     paste0(set_names[1],"<")  , svg_text)
	svg_text <- gsub("name_B<",     paste0(set_names[2],"<")  , svg_text)
	svg_text <- gsub("name_C<",     paste0(set_names[3],"<")  , svg_text)
	svg_text <- gsub("name_D<",     paste0(set_names[4],"<")  , svg_text)
	
	#change font of set names
	maxFont <- 44
	baseFont <- 12
	nchars <- 2*sapply(set_names,nchar)
	nchars[nchars>maxFont]  <- maxFont
	fontSize_sets <- baseFont + (maxFont- nchars)
	print(fontSize_sets)
	
	svg_text <- gsub("font-size:fontSize_Apx",     paste0("font-size:",fontSize_sets[1],"px")  , svg_text)
	svg_text <- gsub("font-size:fontSize_Bpx",     paste0("font-size:",fontSize_sets[2],"px")  , svg_text)
	svg_text <- gsub("font-size:fontSize_Cpx",     paste0("font-size:",fontSize_sets[3],"px")  , svg_text)
	svg_text <- gsub("font-size:fontSize_Dpx",     paste0("font-size:",fontSize_sets[4],"px")  , svg_text)
	
	#change opacity of the fields
	if 	(	length(regmatches(svg_text,regexpr("opacity:opacity_A;",svg_text)))) {
		svg_text <- gsub("opacity:opacity_A;",     paste0("opacity:", sets[2,]$A,";")  , svg_text)
		svg_text <- gsub("opacity:opacity_B;",     paste0("opacity:", sets[2,]$B,";")  , svg_text)
		svg_text <- gsub("opacity:opacity_C;",     paste0("opacity:", sets[2,]$C,";")  , svg_text)
		svg_text <- gsub("opacity:opacity_D;",     paste0("opacity:", sets[2,]$D,";")  , svg_text)
		svg_text <- gsub("opacity:opacity_AB;",    paste0("opacity:", sets[2,]$AB,";") , svg_text)
		svg_text <- gsub("opacity:opacity_AC;",    paste0("opacity:", sets[2,]$AC,";") , svg_text)
		svg_text <- gsub("opacity:opacity_BC;",    paste0("opacity:", sets[2,]$BC,";") , svg_text)
		svg_text <- gsub("opacity:opacity_AD;",     paste0("opacity:", sets[2,]$AD,";")  , svg_text)
		svg_text <- gsub("opacity:opacity_BD;",     paste0("opacity:", sets[2,]$BD,";")  , svg_text)
		svg_text <- gsub("opacity:opacity_CD;",     paste0("opacity:", sets[2,]$CD,";")  , svg_text)
		svg_text <- gsub("opacity:opacity_ABC;",   paste0("opacity:", sets[2,]$ABC,";"), svg_text)
		svg_text <- gsub("opacity:opacity_ABD;",   paste0("opacity:", sets[2,]$ABD,";"), svg_text)
		svg_text <- gsub("opacity:opacity_ACD;",   paste0("opacity:", sets[2,]$ACD,";"), svg_text)
		svg_text <- gsub("opacity:opacity_BCD;",   paste0("opacity:", sets[2,]$BCD,";"), svg_text)
		svg_text <- gsub("opacity:opacity_ABCD;",   paste0("opacity:", sets[2,]$ABCD,";"), svg_text)	
	} else if (	length(regmatches(svg_text,regexpr("opacity:opacity_A\"",svg_text)))){
		svg_text <- gsub("opacity:opacity_A\"",     paste0("opacity:", sets[2,]$A,"\"")  , svg_text)
		svg_text <- gsub("opacity:opacity_B\"",     paste0("opacity:", sets[2,]$B,"\"")  , svg_text)
		svg_text <- gsub("opacity:opacity_C\"",     paste0("opacity:", sets[2,]$C,"\"")  , svg_text)
		svg_text <- gsub("opacity:opacity_D\"",     paste0("opacity:", sets[2,]$D,"\"")  , svg_text)
		svg_text <- gsub("opacity:opacity_AB\"",    paste0("opacity:", sets[2,]$AB,"\"") , svg_text)
		svg_text <- gsub("opacity:opacity_AC\"",    paste0("opacity:", sets[2,]$AC,"\"") , svg_text)
		svg_text <- gsub("opacity:opacity_BC\"",    paste0("opacity:", sets[2,]$BC,"\"") , svg_text)
		svg_text <- gsub("opacity:opacity_AD\"",     paste0("opacity:", sets[2,]$AD,"\"")  , svg_text)
		svg_text <- gsub("opacity:opacity_BD\"",     paste0("opacity:", sets[2,]$BD,"\"")  , svg_text)
		svg_text <- gsub("opacity:opacity_CD\"",     paste0("opacity:", sets[2,]$CD,"\"")  , svg_text)
		svg_text <- gsub("opacity:opacity_ABC\"",   paste0("opacity:", sets[2,]$ABC,"\""), svg_text)
		svg_text <- gsub("opacity:opacity_ABD\"",   paste0("opacity:", sets[2,]$ABD,"\""), svg_text)
		svg_text <- gsub("opacity:opacity_ACD\"",   paste0("opacity:", sets[2,]$ACD,"\""), svg_text)
		svg_text <- gsub("opacity:opacity_BCD\"",   paste0("opacity:", sets[2,]$BCD,"\""), svg_text)
		svg_text <- gsub("opacity:opacity_ABCD\"",   paste0("opacity:", sets[2,]$ABCD,"\""), svg_text)
	}
	else{
		print("Error: cant identify opacity tags in svg_base template")
	}
	
	
	#change values in the subsets
	if 	(!hide_values) {
		svg_text <- gsub("value_A<",     paste0(sets[1,]$A,"<")  , svg_text)
		svg_text <- gsub("value_B<",     paste0(sets[1,]$B,"<")  , svg_text)
		svg_text <- gsub("value_C<",     paste0(sets[1,]$C,"<")  , svg_text)
		svg_text <- gsub("value_D<",     paste0(sets[1,]$D,"<")  , svg_text)
		svg_text <- gsub("value_AB<",    paste0(sets[1,]$AB,"<") , svg_text)
		svg_text <- gsub("value_AC<",    paste0(sets[1,]$AC,"<") , svg_text)
		svg_text <- gsub("value_BC<",    paste0(sets[1,]$BC,"<") , svg_text)
		svg_text <- gsub("value_AD<",    paste0(sets[1,]$AD,"<") , svg_text)
		svg_text <- gsub("value_BD<",    paste0(sets[1,]$BD,"<") , svg_text)
		svg_text <- gsub("value_CD<",    paste0(sets[1,]$CD,"<") , svg_text)
		svg_text <- gsub("value_ABC<",   paste0(sets[1,]$ABC,"<"), svg_text)
		svg_text <- gsub("value_ACD<",   paste0(sets[1,]$ACD,"<"), svg_text)
		svg_text <- gsub("value_ABD<",   paste0(sets[1,]$ABD,"<"), svg_text)
		svg_text <- gsub("value_BCD<",   paste0(sets[1,]$BCD,"<"), svg_text)
		svg_text <- gsub("value_ABCD<",   paste0(sets[1,]$ABCD,"<"), svg_text)
	} else {
		svg_text <- gsub("value_[ABCD]*<",     paste0("<")  ,  svg_text)
	}
	

	
	
	#save updated svg
	file_path <- paste0(getwd(),"/",file_out, ".svg")
	write(svg_text, file = file_path)
	cat("Saved ", type, " venn, colored in ", color, ", to ",file_path, sep = "")
}


#####
unlist_F <- function(x){
	unlist(x, use.names = FALSE)
}

####
ShadyVenn.get_colors <- function(x){
	cat("List of available colors: ")		
	cat(names(ShadyVenn(1,1,1,"just_color")), sep = "\t-\t")
}


#### get basic SVG template.
Venn4er_base <- function() {
	return( 
"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<!-- Created with Inkscape (http://www.inkscape.org/) -->

<svg
	xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
	xmlns:cc=\"http://creativecommons.org/ns#\"
	xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
	xmlns:svg=\"http://www.w3.org/2000/svg\"
	xmlns=\"http://www.w3.org/2000/svg\"
	xmlns:sodipodi=\"http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd\"
	xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\"
	width=\"745\"
	height=\"550\"
	id=\"svg3047\"
	version=\"1.1\"
	inkscape:version=\"0.48.4 r9939\"
	sodipodi:docname=\"4erVenn_base3.svg\">
	<defs
		id=\"defs3049\" />
		<sodipodi:namedview
			id=\"base\"
			pagecolor=\"#ffffff\"
			bordercolor=\"#666666\"
			borderopacity=\"1.0\"
			inkscape:pageopacity=\"0.0\"
			inkscape:pageshadow=\"2\"
			inkscape:zoom=\"1.2\"
			inkscape:cx=\"350\"
			inkscape:cy=\"270\"
			inkscape:document-units=\"px\"
			inkscape:current-layer=\"layer1\"
			showgrid=\"false\"
			inkscape:window-width=\"1527\"
			inkscape:window-height=\"877\"
			inkscape:window-x=\"65\"
			inkscape:window-y=\"-8\"
			inkscape:window-maximized=\"0\" />
			<metadata
				id=\"metadata3052\">
				<rdf:RDF>
					<cc:Work
						rdf:about=\"\">
						<dc:format>image/svg+xml</dc:format>
						<dc:type
							rdf:resource=\"http://purl.org/dc/dcmitype/StillImage\" />
							<dc:title />
						</cc:Work>
					</rdf:RDF>
				</metadata>
				<g
					inkscape:label=\"Layer 1\"
					inkscape:groupmode=\"layer\"
					id=\"layer1\">
					<path
						style=\"fill:color;fill-opacity:1;stroke:#000000;stroke-width:4;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none\"
						d=\"\"
						id=\"path3919\"
						inkscape:connector-curvature=\"0\" />
						<path
							style=\"stroke:#000000;stroke-width:1.0;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none;fill:#ff0000;fill-opacity:opacity_AB\"
		d=\"m 258.0168,271.59187 c -20.16441,-32.27396 -35.44857,-69.90439 -35.37598,-100.84684 -0.53065,-4.44895 0.43212,-3.13905 6.07195,-1.94076 19.00099,6.65118 75.09799,28.54145 74.9628,43.53441 -13.92334,13.54627 -24.85065,31.89121 -35.02898,45.66166 -4.10128,8.0181 -7.92658,16.96822 -10.62979,13.59153 z\"
							id=\"opacity_AB\"
							inkscape:connector-curvature=\"0\"
							inkscape:label=\"opacity_AB\" />
							<path
								style=\"stroke:#000000;stroke-width:1.0;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none;fill:#ff0000;fill-opacity:opacity_B\"
       d=\"m 307.18347,203.03669 c -8.3088,-6.51923 -17.00627,-12.44166 -25.83334,-17.5 -6.45642,-3.68916 -12.54011,-7.09244 -19.44206,-10.24479 -4.93056,-3.9868 -11.52387,-5.34177 -17.2246,-8.08854 -6.70902,-3.61221 -16.79173,-4.13839 -21.66787,-9.01613 5.30429,-22.90558 29.15411,-37.72008 52.0243,-34.88602 31.80835,-0.20822 61.31139,14.81233 87.97691,30.55949 8.59418,5.97892 -13.52294,14.49455 -18.91502,20.7437 -7.73626,7.4241 -16.58133,13.07988 -22.75165,20.93229 -3.73049,2.9429 -8.86079,13.97723 -14.16667,7.5 z\"
								id=\"opacity_B\"
								inkscape:connector-curvature=\"0\"
								inkscape:label=\"opacity_B\" />
								<path
									style=\"stroke:#000000;stroke-width:1.0;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none;fill:#ff0000;fill-opacity:opacity_BC\"
       d=\"m 367.39181,258.40756 c -14.34057,-18.65753 -56.85824,-45.11162 -42.25338,-55.82232 14.28171,-15.70541 22.78985,-22.26302 38.71171,-35.38188 11.83635,-11.87865 23.54932,1.32171 32.26142,7.83936 9.848,11.06523 24.55647,21.46206 32.10069,34.48943 -19.05457,15.08014 -37.09441,34.66913 -53.52954,52.3623 -2.66316,3.32064 -5.25083,-2.44986 -7.2909,-3.48689 z\"
									id=\"opacity_BC\"
									inkscape:connector-curvature=\"0\"
									inkscape:label=\"opacity_BC\" />
									<path
										style=\"stroke:#000000;stroke-width:1.0;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none;fill:#ff0000;fill-opacity:opacity_C\"
       d=\"m 433.01681,202.99536 c -10.89602,-9.79848 -20.96114,-21.57729 -33.29158,-30.1045 -9.02267,-8.10712 -24.48768,-11.20371 -11.70842,-21.52083 6.65581,-2.85494 12.54014,-8.23244 19.51041,-10.24479 10.75897,-6.171 22.77717,-9.91778 34.89551,-13.80502 23.47011,-5.78644 53.7215,-8.43823 70.92148,12.22429 8.4125,9.26677 12.84184,20.41398 -2.72845,24.08073 -7.16928,1.62996 -13.45359,5.11751 -20.09895,7.74479 -5.82694,3.1074 -11.81743,5.47935 -17.5,9.16666 -6.92527,3.75193 -13.45969,7.9229 -20.43677,12.15625 -5.57837,2.76688 -13.57987,14.23725 -19.56323,10.30242 z\"
										id=\"opacity_C\"
										inkscape:connector-curvature=\"0\"
										inkscape:label=\"opacity_C\"
										sodipodi:nodetypes=\"sscssssssscs\" />
										<path
											style=\"stroke:#000000;stroke-width:1.0;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none;fill:#ff0000;fill-opacity:opacity_CD\"
       d=\"m 485.72514,270.93217 c -4.53994,-8.15961 -9.99573,-15.82433 -15.20833,-23.72881 -5.034,-7.33916 -9.31205,-10.36507 -13.33333,-17.45285 -6.4647,-10.28867 -23.26431,-17.91877 -6.03637,-28.62599 9.09205,-4.89421 17.38545,-9.18464 24.8203,-15.09825 5.62745,-2.47937 10.75797,-5.37896 16.21607,-7.98958 6.79847,-2.85484 13.16807,-7.9205 20.63766,-9.51256 19.5035,-6.9656 8.85956,19.50213 9.11271,28.26256 -2.93921,9.73574 -5.03828,18.41498 -8.91704,27.54919 -3.36756,6.99763 -5.60711,14.91004 -9.85152,21.22425 -2.37132,7.14727 -6.52167,12.67709 -10.14848,19.14323 -2.11128,1.52445 -4.09315,12.11724 -7.29167,6.22881 z\"
											id=\"opacity_CD\"
											inkscape:connector-curvature=\"0\"
											inkscape:label=\"opacity_CD\"
											sodipodi:nodetypes=\"ssccscssss\" />
											<path
												style=\"stroke:#000000;stroke-width:1.0;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none;fill:#ff0000;fill-opacity:opacity_BCD\"
       d=\"m 422.39181,344.15146 c -3.62064,-5.85226 -5.0636,-11.46658 -7.95312,-18.12519 -3.74121,-5.68303 -6.32567,-12.85313 -10,-18.45572 -4.0263,-6.16778 -8.68731,-12.93593 -13.08855,-19.38348 -9.77017,-10.55216 -18.23463,-21.89366 -2.42052,-31.81704 14.9474,-13.44537 28.11749,-29.29809 44.84385,-40.48891 11.70859,8.17486 19.77581,21.58217 28.41001,32.75621 4.82772,5.24854 8.92739,12.14786 13.08854,18.16824 2.8017,6.6956 10.8619,13.24092 2.74479,19.56446 -6.88337,10.19806 -15.10127,18.0627 -23.16331,27.14128 -10.71301,9.84769 -20.71111,23.78749 -32.46169,30.64015 z\"
												id=\"opacity_BCD\"
												inkscape:connector-curvature=\"0\"
												inkscape:label=\"opacity_BCD\"
												sodipodi:nodetypes=\"cccsssscc\" />
												<path
													style=\"stroke:#000000;stroke-width:1.0;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none;fill:#ff0000;fill-opacity:opacity_ABCD\"
       d=\"m 371.35014,379.70336 c -7.9729,-3.75725 -15.38136,-10.19206 -22.85239,-15.34375 -7.94449,-5.94713 -19.93153,-10.89056 -14.94224,-24.16809 2.06288,-8.2126 9.19511,-17.03994 14.66963,-27.01594 5.6203,-10.43166 12.25825,-20.34976 18.95834,-30.13889 7.6895,-15.21195 15.85417,7.82276 21.32291,14.60344 7.45845,10.77121 14.02615,25.19876 20.25212,32.95759 3.74928,6.97816 12.87625,14.26978 3.88183,22.34511 -7.06384,7.95626 -16.88972,12.89722 -25.45686,19.26053 -4.97114,2.46344 -10.60481,8.91069 -15.83334,7.5 z\"
													id=\"opacity_ABCD\"
													inkscape:connector-curvature=\"0\"
													inkscape:label=\"opacity_ABCD\" />
													<path
														style=\"stroke:#000000;stroke-width:1.0;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none;fill:#ff0000;fill-opacity:opacity_ABC\"
       d=\"m 303.0074,325.5273 c -12.98676,-13.18201 -25.75475,-25.68404 -36.11709,-40.76792 -5.5564,-9.4811 5.93862,-17.84588 9.11607,-26.32612 6.08795,-7.28143 11.34991,-13.74328 17.25449,-24.43504 14.62751,-17.79474 13.56841,-32.09191 33.08927,-9.29544 14.95483,14.06545 29.12826,28.44704 41.44429,44.85352 -17.57587,21.96791 -29.01516,45.53408 -41.9578,69.43706 -3.56263,6.71066 -18.49954,-11.68904 -22.82923,-13.46606 z\"
														id=\"opacity_ABC\"
														inkscape:connector-curvature=\"0\"
														inkscape:label=\"opacity_ABC\" />
														<path
															style=\"stroke:#000000;stroke-width:1.0;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none;fill:#ff0000;fill-opacity:opacity_AC\"
															d=\"m 292.18347,406.29955 c -12.00079,-0.36413 -31.44795,-6.49047 -39.88077,-14.14599 -28.03624,-24.07712 -4.68131,-113.79247 15.0891,-95.15853 13.02123,13.59443 28.42028,27.97102 40.88404,42.2001 10.04298,8.88447 12.32038,14.48788 10.97961,28.0834 0.007,13.06879 -1.57899,25.37592 -2.28032,37.40101 -7.82639,2.74068 -14.9256,1.77828 -24.79166,1.62001 z\"
															id=\"opacity_AC\"
															inkscape:connector-curvature=\"0\"
															inkscape:label=\"opacity_AC\" />
															<path
																style=\"stroke:#000000;stroke-width:1.0;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none;fill:#ff0000;fill-opacity:opacity_AD\"
       d=\"m 372.77707,445.53669 c -31.37984,-1.0548 -58.00675,-30.4198 -40.9239,-38.50382 7.61284,-3.62076 15.58463,-5.41864 21.78864,-8.27399 6.35426,-3.38375 12.82237,-5.36517 19.13997,-9.26385 5.64175,2.95361 11.1666,5.75369 16.9017,8.4375 7.89264,3.20643 14.49911,5.32261 21.25329,9.0385 16.30733,4.44251 7.28657,17.28669 0.17039,25.59096 -17.66166,11.40499 -15.673,11.73871 -38.33009,12.9747 z\"
																id=\"opacity_AD\"
																inkscape:connector-curvature=\"0\"
																inkscape:label=\"opacity_AD\" />
																<path
																	style=\"stroke:#000000;stroke-width:1.0;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none;fill:#ff0000;fill-opacity:opacity_ACD\"
       d=\"m 321.85597,400.05187 c 1.39209,-10.7868 1.0301,-24.35578 1.78726,-29.30001 1.10666,-9.49448 -2.4407,-18.58521 11.04025,-6.88184 6.88496,5.09316 13.44223,9.95341 20,14.16666 6.33172,3.13762 13.72347,5.45603 4.58475,11.4948 -6.61133,4.37107 -17.54474,7.56344 -25.20864,9.75646 -4.65264,1.26435 -9.02754,3.86644 -12.20362,0.76393 z\"
																	id=\"opacity_ACD\"
																	inkscape:connector-curvature=\"0\"
																	inkscape:label=\"opacity_ACD\"
																	sodipodi:nodetypes=\"cscccc\" />
																	<g
																		id=\"opacity_A\"
																		inkscape:label=\"opacity_A\"
																		style=\"stroke-width:1.0;fill:#ff0000;stroke:#000000;stroke-opacity:1;stroke-miterlimit:4;stroke-dasharray:none;fill-opacity:1\">
																		<g
																			id=\"g4209\"
																			style=\"stroke-width:1.0;fill:#ff0000;stroke:#000000;stroke-opacity:1;stroke-miterlimit:4;stroke-dasharray:none\">
																			<path
																				style=\"stroke:#000000;stroke-width:1.0;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none;fill:#ff0000;fill-opacity:opacity_A\"
         d=\"m 221.55847,364.98947 c -18.87757,-18.05266 -33.76641,-38.47888 -47.70834,-58.61944 -7.5758,-12.53892 -13.9855,-25.86637 -19.86722,-38.06251 -6.16643,-15.03675 -11.37293,-30.73264 -12.17076,-47.12467 -1.82491,-17.11624 5.55199,-42.14683 17.74328,-50.71907 13.43848,-7.66732 27.25482,-8.66965 40.64032,-8.17158 5.26303,1.28352 15.67172,-2.08299 14.72209,8.45283 0.096,24.89728 6.29802,43.94871 14.76563,64.79166 6.5654,14.34347 14.28028,27.86506 22.5,40 6.10105,5.69726 -1.50967,13.36466 -3.33334,19.16667 -3.64571,7.02781 -6.79799,15.34944 -9.16666,23.33333 -5.8811,16.5209 -6.31952,34.35666 -5.12135,51.66667 1.25434,4.07593 4.83442,14.12168 -1.15421,6.9611 -4.00172,-3.8385 -7.93464,-7.74812 -11.84944,-11.67499 z\"
																				id=\"path3129-4\"
																				inkscape:connector-curvature=\"0\"
																				sodipodi:nodetypes=\"cssccscssccc\" />
																				<path
																					style=\"stroke:#000000;stroke-width:1.0;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none;fill:#ff0000;fill-opacity:opacity_A\"
         d=\"m 326.35014,436.61167 c -10.33682,-2.13858 -31.71806,-12.51889 -43.16106,-24.40296 3.51091,-0.69542 7.11939,0.72296 10.67367,0.4329 6.96518,-0.35148 16.4148,0.26896 23.35219,-0.41519 0.77567,3.54811 1.97058,7.6434 3.59188,11.37128 2.40458,4.34403 3.66286,8.71274 6.76873,12.12077 0.71045,1.27934 -0.34011,1.22888 -1.22541,0.8932 z\"
																					id=\"path3923-1\"
																					inkscape:connector-curvature=\"0\" />
																				</g>
																			</g>
																			<path
																				style=\"stroke:#000000;stroke-width:1.0;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none;fill:#ff0000;fill-opacity:opacity_BD\"
       d=\"m 435.78183,406.12595 c -13.42722,0.10153 -6.20801,-18.72421 -7.23865,-29.17808 0.38338,-14.53468 -4.36021,-26.83929 9.48617,-35.68921 17.60979,-15.72527 31.91975,-34.06216 46.75308,-51.92936 2.87187,-6.66162 5.98985,8.21464 8.23438,11.20737 2.72975,7.61576 6.82869,14.74611 7.7239,23.1875 5.49898,17.7237 4.39423,37.00439 1.30374,55.205 -3.56199,8.73472 -9.19597,16.77594 -18.50039,20.77417 -12.77755,7.75071 -32.56994,7.91831 -47.76223,6.42261 z\"
																				id=\"opacity_BD\"
																				inkscape:connector-curvature=\"0\"
																				inkscape:label=\"opacity_BD\" />
																				<path
																					style=\"stroke:#000000;stroke-width:1.0;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none;fill:#ff0000;fill-opacity:opacity_D\"
         d=\"m 509.19682,378.372 c 4.14955,-19.31925 3.29058,-40.31954 -2.39212,-58.85466 -3.0943,-9.75437 -6.07341,-19.04712 -11.0431,-27.21503 -1.45595,-7.58188 -8.44124,-12.78746 -0.24479,-19.26562 4.20777,-6.92365 8.61342,-13.83114 12.29935,-20.97222 7.16292,-12.18393 12.52435,-24.89149 16.77614,-38.19444 5.32097,-14.04722 9.53654,-29.77452 9.50329,-45.13932 5.5412,-11.41351 27.66961,-6.15625 39.65559,-3.43881 9.21537,2.20693 18.35024,8.18915 23.06511,17.13542 8.75669,16.60056 9.37366,37.50994 4.96436,55.94569 -2.82657,11.39137 -5.98849,22.85778 -10.43051,32.99702 -2.77169,5.55921 -5.56163,11.10745 -8.33333,16.66666 -2.80458,6.3191 -6.99267,12.15743 -11.17709,17.89657 -4.96618,8.63737 -10.99673,17.19649 -17.15624,24.83613 -13.09808,17.57676 -28.82029,33.96185 -45.47937,48.0856 l -0.007,-0.48299 z\"
																					id=\"opacity_D\"
																					inkscape:connector-curvature=\"0\"
																					sodipodi:nodetypes=\"csscscccsscc\" />
																					<path
																						style=\"fill:#ff0000;stroke:#000000;stroke-width:1.0;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none;fill-opacity:opacity_D\"
         d=\"m 418.01681,436.47303 c 5.51232,-6.81112 11.24558,-15.80352 11.66667,-24.4177 l 5.20833,0.53162 c 2.86458,0.2924 10.64583,0.79446 17.29167,1.11569 11.26868,0.54468 10.05931,-0.29644 9.375,1.72682 -6.74022,6.37437 -21.04185,20.03894 -43.54167,21.04357 z\"
																						id=\"opacity_D\"
																						inkscape:connector-curvature=\"0\"
																						sodipodi:nodetypes=\"sscccsss\" />
																						<path
																							style=\"stroke:#000000;stroke-width:1.0;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none;fill:#ff0000;fill-opacity:opacity_ABD\"
       d=\"m 418.38498,400.45822 c -10.81911,-3.10901 -21.73909,-7.61673 -32.42168,-12.62808 -9.77714,-4.07905 10.43708,-13.18685 20.12407,-19.69448 11.49136,-8.97765 14.90384,-12.2042 16.07227,0.94322 1.17933,10.54996 2.0719,21.39507 1.6905,32.29115 -1.80936,-0.1074 -3.95466,0.39085 -5.46516,-0.91181 z\"
																							id=\"opacity_ABD\"
																							inkscape:connector-curvature=\"0\"
																							inkscape:label=\"opacity_ABD\"
																							sodipodi:nodetypes=\"ssccscs\" />
																							<text
																								xml:space=\"preserve\"
																								style=\"font-size:fontSize_Apx;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial;text-anchor:middle;text-align:center\"
																								x=\"78.284279\"
																								y=\"186.64792\"
																								id=\"name_A\"
       sodipodi:linespacing=\"125%\"><tspan
         sodipodi:role=\"line\"
         id=\"tspan4016\"
         x=\"83.060745\"
         y=\"308.96832\">name_A</tspan></text>
																								<text
																									xml:space=\"preserve\"
																									style=\"font-size:fontSize_Bpx;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial;text-anchor:middle;text-align:center\"
																									x=\"209.09489\"
																									y=\"135.8551\"
																									id=\"name_B\"
       sodipodi:linespacing=\"125%\"><tspan
         sodipodi:role=\"line\"
         id=\"tspan4016-1\"
         x=\"261.32227\"
         y=\"101.1534\">name_B</tspan></text>
																									<text
																										xml:space=\"preserve\"
																										style=\"font-size:fontSize_Cpx;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial;text-anchor:middle;text-align:center\"
																										x=\"442.80121\"
																										y=\"128.98709\"
																										id=\"name_C\"
       sodipodi:linespacing=\"125%\"><tspan
         sodipodi:role=\"line\"
         id=\"tspan4016-2\"
         x=\"465.02863\"
         y=\"101.32918\">name_C</tspan></text>
																										<text
																											xml:space=\"preserve\"
																											style=\"font-size:fontSize_Dpx;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial;stroke-width:2;fill-opacity:1;text-anchor:middle;text-align:center\"
																											x=\"573.98474\"
																											y=\"164.42654\"
																											id=\"name_D\"
       sodipodi:linespacing=\"125%\"><tspan
         sodipodi:role=\"line\"
         id=\"tspan4016-22\"
         x=\"661.4491\"
         y=\"308.96832\">name_D</tspan></text>
																										</g>
<g
     inkscape:groupmode=\"layer\"
     id=\"layer2\"
     inkscape:label=\"value_text\"
     style=\"display:inline\"
     transform=\"translate(25,0)\">
    <text
       xml:space=\"preserve\"
       style=\"font-size:20px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial\"
       x=\"163.05669\"
       y=\"277.15381\"
       id=\"text3011-42-98\"
       sodipodi:linespacing=\"125%\"><tspan
         sodipodi:role=\"line\"
         id=\"tspan3013-77-40\"
         x=\"163.05669\"
         y=\"277.15381\"
         style=\"font-size:18px\">value_A</tspan></text>
    <text
       xml:space=\"preserve\"
       style=\"font-size:20px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial\"
       x=\"301.21384\"
       y=\"386.54022\"
       id=\"text3011-42-0-7\"
       sodipodi:linespacing=\"125%\"><tspan
         sodipodi:role=\"line\"
         id=\"tspan3013-77-2-6\"
         x=\"301.21384\"
         y=\"386.54022\"
         style=\"font-size:18px\">value_ACD</tspan></text>
    <text
       xml:space=\"preserve\"
       style=\"font-size:20px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial\"
       x=\"244.54716\"
       y=\"364.34702\"
       id=\"text3011-42-8-3\"
       sodipodi:linespacing=\"125%\"><tspan
         sodipodi:role=\"line\"
         id=\"tspan3013-77-6-6\"
         x=\"244.54716\"
         y=\"364.34702\"
         style=\"font-size:18px\">value_AC</tspan></text>
    <text
       xml:space=\"preserve\"
       style=\"font-size:20px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial\"
       x=\"270.38046\"
       y=\"277.26367\"
       id=\"text3011-42-02-1\"
       sodipodi:linespacing=\"125%\"><tspan
         sodipodi:role=\"line\"
         id=\"tspan3013-77-4-5\"
         x=\"270.38046\"
         y=\"277.26367\"
         style=\"font-size:18px\">value_ABC</tspan></text>
    <text
       xml:space=\"preserve\"
       style=\"font-size:20px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial\"
       x=\"226.21385\"
       y=\"216.01367\"
       id=\"text3011-42-86-4\"
       sodipodi:linespacing=\"125%\"><tspan
         sodipodi:role=\"line\"
         id=\"tspan3013-77-5-2\"
         x=\"226.21385\"
         y=\"216.01367\"
         style=\"font-size:18px\">value_AB</tspan></text>
    <text
       xml:space=\"preserve\"
       style=\"font-size:20px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial\"
       x=\"274.54718\"
       y=\"164.65379\"
       id=\"text3011-42-09-0\"
       sodipodi:linespacing=\"125%\"><tspan
         sodipodi:role=\"line\"
         id=\"tspan3013-77-0-9\"
         x=\"274.54718\"
         y=\"164.65379\"
         style=\"font-size:18px\">value_B</tspan></text>
    <text
       xml:space=\"preserve\"
       style=\"font-size:20px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial\"
       x=\"333.60638\"
       y=\"216.01367\"
       id=\"value_BC\"
       sodipodi:linespacing=\"125%\"><tspan
         sodipodi:role=\"line\"
         id=\"tspan3013-77-1-3\"
         x=\"333.60638\"
         y=\"216.01367\"
         style=\"font-size:18px\">value_BC</tspan></text>
    <text
       xml:space=\"preserve\"
       style=\"font-size:20px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial\"
       x=\"417.04718\"
       y=\"164.76366\"
       id=\"text3011-42-3-7\"
       sodipodi:linespacing=\"125%\"><tspan
         sodipodi:role=\"line\"
         id=\"tspan3013-77-8-2\"
         x=\"417.04718\"
         y=\"164.76366\"
         style=\"font-size:18px\">value_C</tspan></text>
    <text
       xml:space=\"preserve\"
       style=\"font-size:20px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial\"
       x=\"384.54715\"
       y=\"277.26367\"
       id=\"text3011-42-9-6\"
       sodipodi:linespacing=\"125%\"><tspan
         sodipodi:role=\"line\"
         id=\"tspan3013-77-3-0\"
         x=\"395\"
         y=\"277\"
         style=\"font-size:18px\">value_BCD</tspan></text>
    <text
       xml:space=\"preserve\"
       style=\"font-size:20px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial\"
       x=\"445.38049\"
       y=\"216.01367\"
       id=\"text3011-42-4-1\"
       sodipodi:linespacing=\"125%\"><tspan
         sodipodi:role=\"line\"
         id=\"tspan3013-77-46-6\"
         x=\"445.38049\"
         y=\"216.01367\"
         style=\"font-size:18px\">value_CD</tspan></text>
    <text
       xml:space=\"preserve\"
       style=\"font-size:20px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial\"
       x=\"511.21384\"
       y=\"277.15381\"
       id=\"text3011-42-066-5\"
       sodipodi:linespacing=\"125%\"><tspan
         sodipodi:role=\"line\"
         id=\"tspan3013-77-18-7\"
         x=\"511.21384\"
         y=\"277.15381\"
         style=\"font-size:18px\">value_D</tspan></text>
    <text
       xml:space=\"preserve\"
       style=\"font-size:20px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial\"
       x=\"425.38049\"
       y=\"364.34702\"
       id=\"text3011-42-49-5\"
       sodipodi:linespacing=\"125%\"><tspan
         sodipodi:role=\"line\"
         id=\"tspan3013-77-63-4\"
         x=\"425.38049\"
         y=\"364.34702\"
         style=\"font-size:18px\">value_BD</tspan></text>
    <text
       xml:space=\"preserve\"
       style=\"font-size:20px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial\"
       x=\"376.21381\"
       y=\"386.43036\"
       id=\"value_ABD\"
       sodipodi:linespacing=\"125%\"><tspan
         sodipodi:role=\"line\"
         id=\"tspan3013-77-88-2\"
         x=\"376.21381\"
         y=\"386.43036\"
         style=\"font-size:18px\">value_ABD</tspan></text>
    <text
       xml:space=\"preserve\"
       style=\"font-size:20px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial\"
       x=\"333.72943\"
       y=\"426.01367\"
       id=\"text3011-42-2-0\"
       sodipodi:linespacing=\"125%\"><tspan
         sodipodi:role=\"line\"
         id=\"tspan3013-77-9-0\"
         x=\"333.72943\"
         y=\"426.01367\"
         style=\"font-size:18px\">value_AD</tspan></text>
    <text
       xml:space=\"preserve\"
       style=\"font-size:20px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial\"
       x=\"321.21381\"
       y=\"341.01367\"
       id=\"text3011-42-1-1\"
       sodipodi:linespacing=\"125%\"><tspan
         sodipodi:role=\"line\"
         id=\"tspan3013-77-35-4\"
         x=\"340\"
         y=\"341.01367\"
         style=\"font-size:18px\">value_ABCD</tspan></text>
  </g>
</svg>")
}


Venn3er_base <- function() {
	return( 
		"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
		<!-- Created with Inkscape (http://www.inkscape.org/) -->
		
		<svg
		xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
		xmlns:cc=\"http://creativecommons.org/ns#\"
		xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
		xmlns:svg=\"http://www.w3.org/2000/svg\"
		xmlns=\"http://www.w3.org/2000/svg\"
		xmlns:sodipodi=\"http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd\"
		xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\"
		width=\"750\"
		height=\"700\"
		id=\"svg3121\"
		version=\"1.1\"
		inkscape:version=\"0.48.4 r9939\"
		sodipodi:docname=\"3erVenn_base.svg\">
		<defs
		id=\"defs3123\" />
		<sodipodi:namedview
		id=\"base\"
		pagecolor=\"#ffffff\"
		bordercolor=\"#666666\"
		borderopacity=\"1.0\"
		inkscape:pageopacity=\"0.0\"
		inkscape:pageshadow=\"2\"
		inkscape:zoom=\"1.2\"
		inkscape:cx=\"350\"
		inkscape:cy=\"200\"
		inkscape:document-units=\"px\"
		inkscape:current-layer=\"layer1\"
		showgrid=\"false\"
		inkscape:window-width=\"1527\"
		inkscape:window-height=\"877\"
		inkscape:window-x=\"65\"
		inkscape:window-y=\"-8\"
		inkscape:window-maximized=\"0\" />
		<metadata
		id=\"metadata3126\">
		<rdf:RDF>
		<cc:Work
		rdf:about=\"\">
		<dc:format>image/svg+xml</dc:format>
		<dc:type
		rdf:resource=\"http://purl.org/dc/dcmitype/StillImage\" />
		<dc:title />
		</cc:Work>
		</rdf:RDF>
		</metadata>
		<g
		inkscape:label=\"Layer 1\"
		inkscape:groupmode=\"layer\"
		id=\"layer1\">
		<path
		style=\"fill:#ff0000;fill-opacity:opacity_AB;stroke:#000000;stroke-width:1.5;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none\"
		d=\"m 366.15179,131.24554 c -44.29534,25.77671 -74.09375,73.74486 -74.09375,128.6875 0.0522,11.8207 1.51953,17.35198 3.03125,24.46875 22.90556,-14.39159 50.01425,-22.71875 79.0625,-22.71875 23.36675,0 45.44801,5.43288 65.125,15.03125 13.75057,-57.1194 -33.36681,-121.27664 -73.125,-145.46875 z\"
		id=\"AB\"
		inkscape:connector-curvature=\"0\"
		sodipodi:nodetypes=\"cccscc\" />
		<path
		style=\"fill:#ff0000;fill-opacity:opacity_C;stroke:#000000;stroke-width:1.5;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none\"
		d=\"m 520.96363,386.24536 c -22.90732,14.39426 -50.01205,22.71875 -79.0625,22.71875 -27.22984,0 -52.76258,-7.32356 -74.71875,-20.09375 -21.96034,12.77687 -47.51249,20.09375 -74.75,20.09375 -23.73415,0 -46.15698,-5.58528 -66.0625,-15.46875 -0.65171,5.65255 -1,11.39157 -1,17.21875 0,82.18463 66.62787,148.81241 148.8125,148.81241 82.18463,0 148.78125,-66.62778 148.78125,-148.81241 0,-8.33542 -0.68311,-16.50667 -2,-24.46875 z\"
		id=\"C\"
		inkscape:connector-curvature=\"0\" />
		<path
		style=\"fill:#ff0000;fill-opacity:opacity_BC;stroke:#000000;stroke-width:1.5;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none\"
		d=\"m 439.72768,276.41965 c -5.48816,47.65558 -31.34264,88.81245 -70.95089,111.85714 21.96063,12.78038 47.47312,20.09375 74.71875,20.09375 29.05636,0 56.18413,-8.31975 79.09375,-22.71875 -5.83735,-52.9511 -71.3752,-107.73705 -82.86161,-109.23214 z\"
		id=\"BC\"
		inkscape:connector-curvature=\"0\"
		sodipodi:nodetypes=\"ccscc\" />
		<path
		style=\"fill:#ff0000;fill-opacity:opacity_AC;stroke:#000000;stroke-width:1.5;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none\"
		d=\"m 294.08929,284.43304 c -37.31404,23.44083 -63.49553,62.94602 -68.78125,108.84375 19.91804,9.8946 42.37599,15.46875 66.125,15.46875 27.24039,0 52.75899,-7.31237 74.71875,-20.09375 -37.57597,-21.86227 -64.69144,-59.7027 -72.0625,-104.21875 z\"
		id=\"AC\"
		inkscape:connector-curvature=\"0\"
		sodipodi:nodetypes=\"ccscc\" />
		<path
		style=\"fill:#ff0000;fill-opacity:opacity_A;stroke:#000000;stroke-width:1.5;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none\"
		d=\"m 290.37917,109.76823 c -82.18464,0 -148.8125,66.62787 -148.8125,148.8125 0,58.44053 33.70154,108.99876 82.71874,133.34375 5.28613,-45.8961 35.01475,-84.00446 68.75001,-108.875 -1.31813,-7.96302 -2.50783,-16.14605 -2.03125,-24.46875 3.03046,-52.92233 29.79841,-102.91079 74.09375,-128.6875 -21.96278,-12.78076 -47.47676,-20.125 -74.71875,-20.125 z\"
		id=\"A\"
		inkscape:connector-curvature=\"0\"
		sodipodi:nodetypes=\"ssccscs\" />
		<path
		style=\"fill:#ff0000;fill-opacity:opacity_B;stroke:#000000;stroke-width:1.5;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none\"
		d=\"m 442.83716,112.48669 c -27.22785,0 -52.73181,7.32927 -74.6875,20.09375 44.28257,25.77996 74.0625,73.75443 74.0625,128.6875 0,5.82084 -0.34972,11.57217 -1,17.21875 42.01063,20.87614 72.76861,61.02033 80.6875,108.875 41.897,-26.32634 69.75,-72.95958 69.75,-126.09375 0,-82.18463 -66.62787,-148.78125 -148.8125,-148.78125 z\"
		id=\"B\"
		inkscape:connector-curvature=\"0\" />
		<path
		style=\"fill:#ff0000;fill-opacity:opacity_ABC;stroke:#000000;stroke-width:1.5;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none\"
		d=\"m 373.62724,262.44925 c -29.04825,0 -56.15694,8.32716 -79.0625,22.71875 7.36375,44.52922 34.47798,82.38275 72.0625,104.25 39.60825,-23.04469 67.60559,-63.84442 73.09375,-111.5 -19.91497,-9.89077 -42.34965,-15.46875 -66.09375,-15.46875 z\"
		id=\"ABC\"
		inkscape:connector-curvature=\"0\" />
		<text
		xml:space=\"preserve\"
		style=\"font-size:24px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial;text-anchor:middle;text-align:center\"
		x=\"181.1945\"
		y=\"241.8965\"
		id=\"textA\"
		sodipodi:linespacing=\"125%\"><tspan
		sodipodi:role=\"line\"
		id=\"tspan3048\"
		x=\"210\"
		y=\"240\">value_A</tspan></text>
		<text
		xml:space=\"preserve\"
		style=\"font-size:24px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial;text-anchor:middle;text-align:center\"
		x=\"474.05173\"
		y=\"236.18221\"
		id=\"textB\"
		sodipodi:linespacing=\"125%\"><tspan
		sodipodi:role=\"line\"
		id=\"tspan3052\"
		x=\"515\"
		y=\"240\">value_B</tspan></text>
		<text
		xml:space=\"preserve\"
		style=\"font-size:24px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial;text-anchor:middle;text-align:center\"
		x=\"336.70987\"
		y=\"209.18224\"
		id=\"textAB\"
		sodipodi:linespacing=\"125%\"><tspan
		sodipodi:role=\"line\"
		id=\"tspan3056\"
		x=\"365\"
		y=\"205\">value_AB</tspan></text>
		<text
		xml:space=\"preserve\"
		style=\"font-size:24px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial;text-anchor:middle;text-align:center\"
		x=\"339.57993\"
		y=\"327.75366\"
		id=\"textABC\"
		sodipodi:linespacing=\"125%\"><tspan
		sodipodi:role=\"line\"
		id=\"tspan3060\"
		x=\"365\"
		y=\"330\">value_ABC</tspan></text>
		<text
		xml:space=\"preserve\"
		style=\"font-size:24px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial;text-anchor:middle;text-align:center\"
		x=\"251.1945\"
		y=\"368.89655\"
		id=\"textAC\"
		sodipodi:linespacing=\"125%\"><tspan
		sodipodi:role=\"line\"
		id=\"tspan3064\"
		x=\"270\"
		y=\"375\">value_AC</tspan></text>
		<text
		xml:space=\"preserve\"
		style=\"font-size:24px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial;text-anchor:middle;text-align:center\"
		x=\"435.48026\"
		y=\"364.79395\"
		id=\"textBC\"
		sodipodi:linespacing=\"125%\"><tspan
		sodipodi:role=\"line\"
		id=\"tspan3068\"
		x=\"460\"
		y=\"375\">value_BC</tspan></text>
		<text
		xml:space=\"preserve\"
		style=\"font-size:24px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial;text-anchor:middle;text-align:center\"
		x=\"341.68024\"
		y=\"476.3251\"
		id=\"textC\"
		sodipodi:linespacing=\"125%\"
		inkscape:label=\"#textC\"><tspan
		sodipodi:role=\"line\"
		id=\"tspan3072\"
		x=\"370\"
		y=\"495\">value_C</tspan></text>
		<text
		xml:space=\"preserve\"
		style=\"font-size:fontSize_Apx;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial;text-anchor:middle;text-align:center\"
		x=\"125.71429\"
		y=\"110.93362\"
		id=\"text4238\"
		sodipodi:linespacing=\"125%\"><tspan
		sodipodi:role=\"line\"
		id=\"tspan4240\"
		x=\"100\"
		y=\"100\">name_A</tspan></text>
		<text
		xml:space=\"preserve\"
		style=\"font-size:fontSize_Bpx;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial;text-anchor:middle;text-align:center\"
		x=\"534.26483\"
		y=\"99.535728\"
		id=\"text4238-2\"
		sodipodi:linespacing=\"125%\"><tspan
		sodipodi:role=\"line\"
		id=\"tspan4240-2\"
		x=\"580\"
		y=\"100\">name_B</tspan></text>
		<text
		xml:space=\"preserve\"
		style=\"font-size:fontSize_Cpx;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial;text-anchor:middle;text-align:center\"
		x=\"324.2648\"
		y=\"603.82147\"
		id=\"text4238-1\"
		sodipodi:linespacing=\"125%\"><tspan
		sodipodi:role=\"line\"
		id=\"tspan4240-6\"
		x=\"375\"
		y=\"615\">name_C</tspan></text>
		</g>
		</svg>
		")
}


Venn2er_base <- function() {
	return( 
		"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
		<!-- Created with Inkscape (http://www.inkscape.org/) -->
		
		<svg
		xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
		xmlns:cc=\"http://creativecommons.org/ns#\"
		xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
		xmlns:svg=\"http://www.w3.org/2000/svg\"
		xmlns=\"http://www.w3.org/2000/svg\"
		xmlns:sodipodi=\"http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd\"
		xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\"
		width=\"750\"
		height=\"550\"
		id=\"svg3009\"
		version=\"1.1\"
		inkscape:version=\"0.48.4 r9939\"
		sodipodi:docname=\"Venn2er_base.svg\">
		<defs
		id=\"defs3011\" />
		<sodipodi:namedview
		id=\"base\"
		pagecolor=\"#ffffff\"
		bordercolor=\"#666666\"
		borderopacity=\"1.0\"
		inkscape:pageopacity=\"0.0\"
		inkscape:pageshadow=\"2\"
		inkscape:zoom=\"1.5004828\"
		inkscape:cx=\"350\"
		inkscape:cy=\"300\"
		inkscape:document-units=\"px\"
		inkscape:current-layer=\"layer1\"
		showgrid=\"false\"
		inkscape:window-width=\"1527\"
		inkscape:window-height=\"877\"
		inkscape:window-x=\"65\"
		inkscape:window-y=\"-8\"
		inkscape:window-maximized=\"0\" />
		<metadata
		id=\"metadata3014\">
		<rdf:RDF>
		<cc:Work
		rdf:about=\"\">
		<dc:format>image/svg+xml</dc:format>
		<dc:type
		rdf:resource=\"http://purl.org/dc/dcmitype/StillImage\" />
		<dc:title></dc:title>
		</cc:Work>
		</rdf:RDF>
		</metadata>
		<g
		inkscape:label=\"Layer 1\"
		inkscape:groupmode=\"layer\"
		id=\"layer1\">
		<path
		inkscape:connector-curvature=\"0\"
		style=\"opacity:0.8;fill:#ff0000;fill-opacity:opacity_B;stroke:#000000;stroke-width:1;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none\"
		d=\"m 460.34451,107.4375 c -27.46194,0 -53.18553,7.40847 -75.3125,20.3125 44.50818,25.95362 74.4375,74.19969 74.4375,129.4375 0,55.23781 -29.92932,103.48388 -74.4375,129.4375 22.12697,12.90403 47.85056,20.3125 75.3125,20.3125 82.69732,0 149.75,-67.05268 149.75,-149.75 0,-82.69732 -67.05268,-149.75 -149.75,-149.75 z\"
		id=\"opacity_B\"
		inkscape:label=\"opacity_B\" />
		<path
		inkscape:connector-curvature=\"0\"
		style=\"opacity:0.8;fill:#ff0000;fill-opacity:opacity_A;stroke:#000000;stroke-width:1;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none\"
		d=\"m 309.21951,107.53125 c -82.69732,0 -149.71875,67.05268 -149.71875,149.75 0,82.69732 67.02143,149.75 149.71875,149.75 27.45951,0 53.18685,-7.41059 75.3125,-20.3125 -44.49539,-25.95685 -74.40625,-74.20928 -74.40625,-129.4375 0,-55.22822 29.91086,-103.48065 74.40625,-129.4375 -22.12565,-12.90191 -47.85299,-20.3125 -75.3125,-20.3125 z\"
		id=\"opacity_A\"
		inkscape:label=\"opacity_A\" />
		<path
		style=\"opacity:0.8;fill:#ff0000;fill-opacity:opacity_AB;stroke:#000000;stroke-width:1.01015258;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none\"
		d=\"m 376.15167,378.44212 c -11.6993,-7.88909 -30.92334,-27.36443 -38.75688,-39.26352 -28.90832,-43.91154 -33.00481,-96.65934 -11.20289,-144.25217 10.07566,-21.99481 30.27418,-45.79054 50.04081,-58.95272 9.86041,-6.56584 8.50573,-6.76769 22.11569,3.2954 30.85809,22.81625 51.96267,58.42393 57.84478,97.59581 1.389,9.25006 1.389,31.36892 0,40.61898 -6.09278,40.5749 -26.79506,74.82358 -59.92316,99.13351 -11.45808,8.40812 -10.48606,8.31996 -20.11835,1.82471 z\"
		id=\"opacity_AB\"
		inkscape:connector-curvature=\"0\"
		sodipodi:nodetypes=\"cssssssscc\"
		inkscape:label=\"opacity_AB\" />
		<text
		xml:space=\"preserve\"
		style=\"font-size:fontSize_Apx;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial;text-anchor:middle;text-align:center\"
		x=\"81.762344\"
		y=\"108.87971\"
		id=\"name_A\"
		sodipodi:linespacing=\"125%\"
		inkscape:label=\"name_A\"><tspan
		sodipodi:role=\"line\"
		id=\"tspan3039\"
		x=\"120\"
		y=\"108\">name_A</tspan></text>
		<text
		xml:space=\"preserve\"
		style=\"font-size:fontSize_Bpx;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial;text-anchor:middle;text-align:center\"
		x=\"574.71674\"
		y=\"108.87971\"
		id=\"name_B\"
		sodipodi:linespacing=\"125%\"
		inkscape:label=\"name_B\"><tspan
		sodipodi:role=\"line\"
		id=\"tspan3043\"
		x=\"600\"
		y=\"108\">name_B</tspan></text>
		<text
		xml:space=\"preserve\"
		style=\"font-size:30px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial;text-anchor:middle;text-align:center\"
		x=\"203.99081\"
		y=\"266.86621\"
		id=\"value_A\"
		sodipodi:linespacing=\"125%\"
		inkscape:label=\"value_A\"><tspan
		sodipodi:role=\"line\"
		id=\"tspan3047\"
		x=\"235.99081\"
		y=\"266.86621\"
		style=\"font-size:25px\">value_A</tspan></text>
		<text
		xml:space=\"preserve\"
		style=\"font-size:30px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial;text-anchor:middle;text-align:center\"
		x=\"334.33908\"
		y=\"266.86621\"
		id=\"value_AB\"
		sodipodi:linespacing=\"125%\"
		inkscape:label=\"value_AB\"><tspan
		sodipodi:role=\"line\"
		id=\"tspan3047-4\"
		x=\"375.33908\"
		y=\"266.86621\"
		style=\"font-size:25px\">value_AB</tspan></text>
		<text
		xml:space=\"preserve\"
		style=\"font-size:30px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial;text-anchor:middle;text-align:center\"
		x=\"464.68738\"
		y=\"266.86621\"
		id=\"value_B\"
		sodipodi:linespacing=\"125%\"
		inkscape:label=\"value_B\"><tspan
		sodipodi:role=\"line\"
		id=\"tspan3047-9\"
		x=\"510.68738\"
		y=\"266.86621\"
		style=\"font-size:25px\">value_B</tspan></text>
		</g>
		</svg>")
}
