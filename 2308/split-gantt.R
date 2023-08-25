
#-------------------------------------
CRAN_US <- 'http://cran.us.r-project.org'; req <- c("ggplot2", "gridExtra"); ipack <- rownames( installed.packages() ); 
for(pk in req) { if( !(pk %in% ipack) ) install.packages(pk, repos=CRAN_US); }
library(ggplot2); library(gridExtra);


#-------------------------------------
add_gantt_extras <- function(df, mid="") {
	d1 <- min(df$start);	
	df$x1 <- date_diff(d1, df$start);
	df$x2 <- date_diff(d1, df$end);
    if( nchar(mid) == 0 ) return(df);

    mid <- date_diff(d1, mid);

    for(k in 1:nrow(df)) {
        x1 <- df$x1[k]; x2 <- df$x2[k];
        if(mid > df$x2[k]) { df[k, "b1"] <- x1; df[k, "b2"] <- x2; df[k, "a1"] <- NA; df[k, "a2"] <- NA; }
        else if(mid < df$x1[k]) { df[k, "b1"] <- NA; df[k, "b2"] <- NA; df[k, "a1"] <- x1; df[k, "a2"] <- x2; }
        else {  df[k, "b1"] <- x1; df[k, "b2"] <- mid; df[k, "a1"] <- mid; df[k, "a2"] <- x2; }
    } 
		
	return(df);
}

#-------------------------------------
geom_head <- function(df, top=7) {  
	annotation_custom(tableGrob(head(df, top)), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf); 
}  

#-------------------------------------------
date_diff <- function(d1, d2, units="days") {
	as.double( difftime(strptime(d2, format = "%Y-%m-%d"),
		strptime(d1, format = "%Y-%m-%d"), units=units) );
}

#-------------------------------------
gantt_bar <- function(size=7) {
	geom_segment(aes(x=x1, xend=x2, y=code, yend=code, color=code), size=size, show.legend=FALSE);
}

#-------------------------------------
gantt_text_mid <- function() {
	geom_text(aes(x=(x1+x2)/2, y=code, label=code), show.legend=FALSE);
}

#-------------------------------------
gantt_text_head <- function(dy=-1) {
	geom_text(aes(x=(x1*0.75 + x2*0.25), y=code, label=start), vjust=dy, show.legend=FALSE);
}

#-------------------------------------
gantt_text_tail <- function(dy=+1.75) { 
	geom_text(aes(x=(x2*0.75 + x1*0.25), y=code, label=end), vjust=dy, show.legend=FALSE); 
}

