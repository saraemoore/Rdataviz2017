## ----knitr_setup, echo=FALSE---------------------------------------------
showtext::showtext.opts(dpi = 300)

## ----whodataexample, eval=FALSE------------------------------------------
## library(WHO)
## codes <- get_codes()
## who_data <- get_data("WHS9_CBR")

## ----getcodes, cache=TRUE, echo=FALSE, message=FALSE---------------------
library(WHO)
codes <- get_codes(extra = TRUE)

## ----exploredata, echo=FALSE, eval=FALSE---------------------------------
## categoryWHOcodes <- function(who_cat, all_codes) {
## 	as.data.frame(subset(all_codes,
## 						 category==who_cat,
## 						 select = c(label, display)))
## }
##
## unique(codes$category)
##
## categoryWHOcodes("Demographic and socioeconomic statistics", codes)
## categoryWHOcodes("Injuries and violence", codes)
## categoryWHOcodes("Sustainable development goals", codes)
## categoryWHOcodes("Mortality and global health estimates", codes)

## ----grabdata, echo=FALSE, cache=TRUE------------------------------------
getWHOdataByLabels <- function(who_labels) {
	who_data = lapply(who_labels, get_data)
	names(who_data) = who_labels
	# remove empty datasets
	no_data = (lapply(who_data, nrow)==0)
	if(any(no_data)) {
		who_data = who_data[-unname(which(no_data))]
	}
	return(who_data)
}

labelWHOcodes <- function(selected_labels, all_codes) {
	as.data.frame(subset(all_codes,
						 label%in%selected_labels,
						 select = c(label, display)))
}
makeWHOnamevec <- function(selected_labels, all_codes) {
	selected_codes <- labelWHOcodes(selected_labels, all_codes)
	# make a name lookup table
	selected_names = selected_codes$display
	names(selected_names) = selected_codes$label
	return(selected_names)
}

data_labels = c("WHS9_CBR", "WHS9_CDR", "MDG_0000000001", "MDG_0000000026", "WHS2_126")

who_data = getWHOdataByLabels(data_labels)
who_names = makeWHOnamevec(names(who_data), codes)

## ----cleandata, echo=FALSE, cache=TRUE, warning=FALSE, message=FALSE-----
library(dplyr)

splitValueCol = function(df_col, split_on) {
	bind_rows(lapply(df_col, function(x) as.data.frame(t(as.numeric(unlist(strsplit(split = split_on, x)))))))
}
splitValueColNames = c("value", "value_lower", "value_upper")

who_data[["MDG_0000000001"]][,splitValueColNames] <- splitValueCol(
	as.data.frame(who_data[["MDG_0000000001"]])$value,
	"[[:space:]]\\[|-|\\]")

# get rid of pesky spaces
who_data[["MDG_0000000026"]]$value = gsub("(\\d+)\\s+(\\d+)", "\\1\\2", who_data[["MDG_0000000026"]]$value)
who_data[["MDG_0000000026"]][,splitValueColNames] <- splitValueCol(
	as.data.frame(who_data[["MDG_0000000026"]])$value,
	"[[:space:]]\\[[[:space:]]|[[:space:]]-[[:space:]]+|\\]")

# quick and dirty way to make all strings into factors
who_data = lapply(who_data, function(x) as.data.frame(unclass(x)))

who_cbdr = merge(who_data[["WHS9_CBR"]], who_data[["WHS9_CDR"]],
				 by = c("year", "worldbankincomegroup", "country", "region"),
				 all = TRUE,
				 suffixes = c(".birthsper1000",".deathsper1000"))

who_mort = merge(who_data[["MDG_0000000001"]], who_data[["MDG_0000000026"]],
				 by = c("year", "country", "region"),
				 all = TRUE,
				 suffixes = c(".infant", ".maternal"))
who_mort[,c("value.infant", "value_lower.infant", "value_upper.infant")] = who_mort[,c("value.infant", "value_lower.infant", "value_upper.infant")]/1000
who_mort[,c("value.maternal", "value_lower.maternal", "value_upper.maternal")] = who_mort[,c("value.maternal", "value_lower.maternal", "value_upper.maternal")]/100000

# http://apps.who.int/gho/indicatorregistry/App_Main/view_indicator.aspx?iid=90
who_yllmajorcause = as.data.frame(subset(who_data[["WHS2_126"]], year==2012) %>%
						   group_by(region, ghecauses) %>%
						   summarise(med.pct = median(value)))

## ----simple_scatterplot0, message=FALSE----------------------
library(ggplot2)
p = ggplot(data = subset(who_cbdr, !is.na(country)),
		aes(x = value.birthsper1000,
			y = value.deathsper1000,
			color = worldbankincomegroup)) +
	geom_point()
p

## ----levels_scatterplot--------------------------------------------------
levels(who_cbdr$worldbankincomegroup)

## ----grouporder_scatterplot0---------------------------------
# reorder factor levels
who_cbdr$worldbankincomegroup = factor(
	who_cbdr$worldbankincomegroup,
	levels = levels(who_cbdr$worldbankincomegroup)[c(2, 5:3, 1)],
	labels = c("High", "Upper-middle", "Lower-middle", "Low", "Global"))

# recreate plot with modifed data
p = p %+% subset(who_cbdr, !is.na(country))
p

## ----label_scatterplot0--------------------------------------
p = p +
	xlab("Crude birth rate") +
	ylab("Crude death rate") +
	labs(title = "Global population rates",
		 subtitle = "2013; per 1,000 population",
		 caption = "Source: UN World Population Prospects (https://esa.un.org/unpd/wpp/)") +
	theme(plot.title = element_text(hjust = 0.5),
		  plot.subtitle = element_text(hjust = 0.5),
		  plot.caption = element_text(size = 8))
p

## ----legend_scatterplot0-------------------------------------
p = p +
	scale_colour_brewer("World Bank Income Group",
						palette = "Dark2") +
	theme(legend.position = "bottom")
p

## ----summary_scatterplot0------------------------------------
p = p +
	stat_ellipse(type = "t", level = 0.9,
				 segments = 80, alpha = 0.5) +
	 geom_smooth(aes(color = NULL), method = "loess",
	 			 span = 0.6, se = FALSE,
	 			 color = "grey40", linetype = "longdash")
p

## ----theme_scatterplot0--------------------------------------
old_theme = theme_set(theme_minimal(base_size = 14))
p

## ----font_scatterplot0---------------------------------------
library(showtext)
sysfonts::font.add.google("Open Sans", "open_sans")
showtext::showtext.auto()
p = p + theme(text = element_text(family = "open_sans"))
p

## ----labelobs_scatterplot0-----------------------------------
library(dplyr)
library(ggrepel)
p = p + geom_label_repel(data = subset(who_cbdr,
									   !is.na(country)) %>%
			group_by(worldbankincomegroup) %>%
			top_n(3, abs(scale(value.birthsper1000)) +
			  		 abs(scale(value.deathsper1000))),
		aes(label = country),
		show.legend = FALSE,
		size = 3.5,
		alpha = 0.65,
		box.padding = unit(0.6, "lines"),
    	point.padding = unit(0.4, "lines"),
    	segment.color = "grey50",
    	segment.alpha = 0.65)
p

## ----reset_plot_settings, echo=FALSE-------------------------------------
showtext.auto(FALSE)
new_theme = theme_set(old_theme)

## ----heatmap0, results='asis', message=FALSE-----------------
wrap_labels = function(x, width = 15) {
	wrapped = strwrap(x, width = width, simplify = FALSE)
	lapply(wrapped, paste, collapse = "\n")
}

p1 = ggplot(who_yllmajorcause,
			aes(x = ghecauses, y = region, fill = med.pct)) +
	geom_tile() +
	scale_fill_distiller("Percent of total",
						 palette = "Blues",
						 direction = 1,
						 breaks = seq(0, 100, 25),
						 limits = c(0, 100),
						 guide = guide_colorbar(barwidth = 15,
						 						barheight = 1)) +
	scale_x_discrete("Cause Group", expand = c(0, 0),
					 labels = wrap_labels) +
	scale_y_discrete("Region", expand = c(0, 0),
					 labels = wrap_labels) +
	ggtitle("Median percent of years of life lost",
			subtitle = "in 2012 by cause and region") +
	theme_minimal(base_size = 16) +
	theme(legend.position = "bottom",
		  plot.title = element_text(hjust = 0.5),
		  plot.subtitle = element_text(hjust = 0.5))
p1

## ----dendro0, results='asis', message=FALSE------------------
library(tidyr)
library(ggdendro)

who_yllmajorcause_mat = who_yllmajorcause %>%
						spread(region, med.pct) # long --> wide

rownames(who_yllmajorcause_mat) = who_yllmajorcause_mat[,"ghecauses"]
col_idx = which(colnames(who_yllmajorcause_mat)=="ghecauses")
who_yllmajorcause_mat = as.matrix(who_yllmajorcause_mat[, -col_idx])

yll_hc = hclust(dist(t(who_yllmajorcause_mat)), "average")
# plot the raw dendrogram
ggdendrogram(yll_hc, rotate = TRUE)

## ----simpledendro0, results='asis', message=FALSE------------
library(grid) # unit

yll_dendro = as.dendrogram(yll_hc)
yll_ddata = dendro_data(yll_dendro)

p2 = ggplot(segment(yll_ddata)) +
		geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
		coord_flip() +
		theme_dendro()
p2

## ----heatmap0ro, results='asis', message=FALSE---------------
# just in case there are extra factor levels, etc., un-factor
old_levels = levels(who_yllmajorcause$region)
who_yllmajorcause$region = as.character(who_yllmajorcause$region)

# re-factor with clustering/dendrogram ordering
new_order = order.dendrogram(yll_dendro)
who_yllmajorcause$region = factor(who_yllmajorcause$region,
								  old_levels[new_order])

# recreate heatmap with modifed data
p1 = p1 %+% who_yllmajorcause
p1

## ----heatmapdendro0, results='asis', message=FALSE-----------
library(gtable)

### dendrogram grob
# tweak these if the dendrogram doesn't line up:
dendro_margin = grid::unit(c(15, 0, -5, -10), "points")
p2_grob = ggplotGrob(p2 + theme(plot.margin = dendro_margin))

### heatmap grob
p1_grob = ggplotGrob(p1)

# add some space for the dendrogram
g1 = gtable_add_cols(p1_grob, grid::unit(4, "cm"))
# adjust top ("t") and bottom ("b") if no title/subtitle:
g = gtable_add_grob(g1,
					p2_grob,
	t = 5, b = 6,
	l = ncol(g1), r = ncol(g1))
grid.newpage()
grid.draw(g)

## ----scatter_revisited0--------------------------------------
p_basic = ggplot(data = subset(who_cbdr, !is.na(country)),
			 aes(x = value.birthsper1000,
				 y = value.deathsper1000,
				 color = worldbankincomegroup)) +
	scale_colour_brewer("World Bank Income Group",
				palette = "Dark2") +
	xlab("Crude birth rate") +
	ylab("Crude death rate") +
	labs(title = "Global population rates",
		 subtitle = "2013; per 1,000 population",
		 caption = "Source: UN World Population Prospects (https://esa.un.org/unpd/wpp/)") +
	theme_minimal(base_size = 14) +
	theme(plot.title = element_text(hjust = 0.5),
		  plot.subtitle = element_text(hjust = 0.5),
		  plot.caption = element_text(size = 8),
		  legend.position = "bottom")

p_basic +
	geom_point()

## ----scatter_ggiraph0, warning=FALSE-------------------------------------
# devtools::install_github("davidgohel/ggiraph")
library(ggiraph)

p_ggiraph = p_basic +
	geom_point_interactive(
		aes(tooltip = paste(gsub("'", "&#39;", country),
							paste(value.birthsper1000,
								  value.deathsper1000,
						  		  sep = ", "),
							sep = ": ")))

## ----scatter_ggiraph1a, message=FALSE, eval=FALSE------------------------
# in an R session, opens in browser
ggiraph(ggobj = p_ggiraph,
		width_svg = 8,
		height_svg = 6,
		tooltip_opacity = 0.7)

## ----plotly0, message=FALSE----------------------------------
library(plotly)

p_plotly = p_basic +
	geom_point(aes(text = country))

ggplotly(p_plotly)

## ----dt0, out.width='100%'-----------------------------------
library(DT)
dont_show_cols = which(grepl("^gho|^publish",
							 colnames(who_cbdr)))
datatable(who_cbdr[,-dont_show_cols],
		  caption = "Global population rates in 2013 per 1,000 population",
		  rownames = FALSE,
		  colnames = c("World Bank Income Group" = 2,
		  			   "Crude birth rate" = 5,
		  			   "Crude death rate" = 6),
		  options = list(pageLength = 5,
		  				 lengthMenu = c(5, 10, 15)))

## ----leaflet0------------------------------------------------
library(leaflet)

df = data.frame(name = "University Hall",
				lat = 37.8719032,
				long = -122.2664164)

leaflet(df) %>%
	addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
	setView(-122.2585399, 37.8718992, zoom = 16) %>%
	addCircleMarkers(~long, ~lat, popup = ~name)

## ----rbokeh0-------------------------------------------------
library(rbokeh)
p <- figure() %>%
    ly_points(value.birthsper1000, value.deathsper1000,
              data = subset(who_cbdr, !is.na(country)),
              color = worldbankincomegroup,
              hover = list(country,
                           value.birthsper1000,
                           value.deathsper1000)) %>%
    x_axis(label = "Crude Birth Rate") %>%
    y_axis(label = "Crude Death Rate")
p

## ----googlevis0a, results='asis', message=FALSE---------------
library(googleVis)
# for knitr:
# data.frame with >=4 cols: x, y, id, time. color and size optional,
# but if you don't provide them,
# it will choose them for you (if there are columns left to use).
# Motion Chart only displayed when hosted on a web server
# or placed in a directory listed in Flash's local trusted sources.
mc = gvisMotionChart(as.data.frame(
		subset(who_mort,
			   !is.na(country)&!is.na(value.maternal),
			   select = c(country, year, region, worldbankincomegroup, value.infant, value.maternal))),
	idvar = "country", timevar = "year",
	xvar = "value.infant", yvar = "value.maternal",
	colorvar = "worldbankincomegroup",
	options = list(width = 750, height = 650))

## ----googlevis1a, message=FALSE, eval=FALSE------------------------------
plot(mc) # in an R session, opens in browser
