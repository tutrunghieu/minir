
#-------------------------------------------
geom_bar_xyfill <- function(pos="stack", legend=TRUE, more=NULL) { 
    geom_bar(aes(x=xx, y=yy, fill=fill), stat="identity", position=pos, show.legend=legend);
} 

#-------------------------------------------
geom_text_xyfill <- function(fmt=fmt_c1_e3, pos=position_stack(vjust=0.5), angle=0) {
	geom_text(aes(x=xx, y=yy, label=fmt(yy), group=fill), position=pos, angle=angle, show.legend=FALSE);
}

#-------------------------------------
bottom_legend <- function(val="bottom") { theme(legend.position=val, legend.title=element_blank()); }

