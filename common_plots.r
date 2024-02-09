# draw a plot with title only
mktitle <- function(titlestring='') {
	plottitle <- ggdraw() + 
		draw_label( titlestring,
								fontface = 'bold',
								x = 0,
								hjust = 0
		) +
		theme(
			# add margin on the left of the drawing canvas,
			# so title is aligned with left edge of first plot
			plot.margin = margin(0, 0, 0, 7)
		)
	return (plottitle)
}



# ----- support funcs -----

# generate group of strings for custom bin/level
# assumes the INCLUSIVE OF LEFT
mk_lv_names <- function (breaks) {
	# breaks is a vector of ORDERED values, should be 1 more than actual number of levels! e.g. 0,0.1,0.2...1
	binsize=length(breaks)-1
	
	# format break range as labels
	breaknames=sprintf('(%s-%s]',breaks[seq(1,binsize)], breaks[seq(2,(binsize+1))])
	# 1st includes values both sides
	breaknames[1]=str_replace(breaknames[1],'\\(','[')
	return (breaknames)
}

# input a vector of full axis names, e.g. 0,0.1,0.2...1, and divide the axis to 4 pars , so only show 5 labels as axis ticks
divide_axis_names_to_4 <- function (bin_names) {
	# bin_names = ordered bin names from left to right on x
	binsize = length(bin_names)
	return ( bin_names[c(
		1, # 1st
		floor(0.25*binsize), # 1/4 point
		floor(median(1:binsize)), # 1/2 point, middle point
		floor(0.75*binsize), # 3/4 point
		binsize # last
		)
	]
	)
}


# convert data table ready for histogram, divide into bins and add conut value
# datatable must have columns: value, subgroup(optional)
datatable_to_hist <- function (datatable, binsize=10, bin_min=NA, bin_max=NA, binwidth=0) {

	# ----- input datatable -----
	# column name for values to be binned, MUST be "value"
	if (!'value' %in% colnames(datatable)) {
		warning ('input data table must have column "value" for processing!')
		return (NULL)
	}

	# ----- decide bins/levels -----
	# NA will be ignored
	if (is.na(bin_min)) {
		bin_min=min(datatable$value, na.rm = TRUE)
	}
	if (is.na(bin_max)) {
		bin_max=max(datatable$value, na.rm = TRUE)
	}
	# if both binsize and binwidth are given, use binwidth
	if (binwidth>0)	{
		custom_break=seq(bin_min,bin_max,binwidth)
	} else {
		custom_break=seq(bin_min,bin_max,(bin_max-bin_min)/binsize)
	}

	breaknames=mk_lv_names(custom_break)
	
	
	# ----- bin values and deal with subgroup (if needed) -----
	# if need to subgroup the data values, put info into column "subgroup"
	# use Base R's hist() to get binned A,B counts
	if ('subgroup' %in% colnames(datatable)) {
		histdata=list()
		data2=datatable %>%
			group_by(subgroup) %>%
			nest()
		for (i in 1:length(data2$data)) {
			h0=hist(data2$data[[i]]$value, plot=F, breaks=custom_break)$counts
			histdata[[ data2$subgroup[[i]] ]] = data.frame(
				bin=breaknames,
				subgroup=data2$subgroup[[i]],
				counts=h0
				)
		}
		histdata=bind_rows(histdata)
	} else {
		histdata=data.frame(
			bin=breaknames,
			counts=hist(datatable$value,plot=F,breaks=custom_break)$counts
		)
	}
	
	return(histdata)
}