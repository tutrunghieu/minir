
library(ggplot2);
library(gridExtra);


gantt_after <- function(gdf, wd) {
  tdf <- data.frame();

  for(k in 1:nrow(gdf)) {
     pk <- paste("c", k);
     sk <- gdf$first[k];
     ek <- gdf$last[k];
     if(ek<wd) { next; }
     tdf[pk, "code"] <- gdf[k, "code"];
     tdf[pk, "tile"] <- gdf[k, "tile"];
     if(sk>wd) { tdf[pk, "first_WL"] <- sk; tdf[pk, "last_WL"] <- ek; }     
     if(sk<wd) { tdf[pk, "first_WL"] <- wd; tdf[pk, "last_WL"] <- ek; }     
  }

  return(tdf);
}

WALE_score <- function(tdf) {
   sprintf("%s: WALE = %0.1f months", max(tdf$tile), mean(ddm(tdf$last_WL, tdf$first_WL)) );
}

ddm <- function(end_date, start_date) {
    ed <- as.POSIXlt(end_date);
    sd <- as.POSIXlt(start_date)
    12 * (ed$year - sd$year) + (ed$mon - sd$mon);
}

gantt_chart <- function(ddd, wd=as.Date("2024-03-15"), dash_col='blue', dash_type="dashed", 
	full_col='yellow', full_size=3.5, wale_col='black', wale_size=0.5) {
      tdf <- gantt_after(ddd, wd);
	ggplot(ddd) + ggtitle( WALE_score(tdf) ) + 
      geom_segment(aes(x=first, xend=last, y=code, yend=code), color=full_col, size=full_size, show.legend=FALSE) +
      geom_segment(data=tdf, aes(x=first_WL, xend=last_WL, y=code, yend=code), color=wale_col, size=wale_size, show.legend=FALSE) +
      geom_vline(aes(xintercept=wd), color=dash_col, linetype=dash_type) +
   	theme(axis.title.x = element_blank(), axis.title.y = element_blank() );
}

gantt_array <- function(gdf, ncol=3, FUN=gantt_chart) {
   ldx <- lapply(split(gdf, gdf$tile), FUN=FUN);
   grid.arrange(grobs=ldx, ncol=ncol);
}

gdf <- dataset;
gdf$first <- as.Date(gdf$first);
gdf$last <- as.Date(gdf$last);

gantt_array(gdf);
