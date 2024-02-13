
INSIDE_PBI <- exists("powerbi_rPngDeviceType");
if(!INSIDE_PBI) { rm(list=ls()); INSIDE_PBI <- FALSE; }


library(ggplot2);
library(gridExtra);

user_file <- function(rel) {
   fp <- file.path(Sys.getenv("USERPROFILE"), rel);
   dir.create(dirname(fp), recursive=TRUE, showWarnings=FALSE);
   return(fp);
}

runif_def <- function(N, max=1, tag="abc") { runif(N, max=max); } 

rcode <- function(N, pref="c", runif=runif_def, tag="code") { 
	sprintf("%s%0.0f%0.0f", pref, 12345678*runif(N, tag=paste(tag, "one")), 12345678*runif(N, tag=paste(tag, " two"))); }

rdate <- function(N, wd, max=2.5*365, runif=runif_def, tag="date") { 
	as.Date(wd) + runif(N, max=abs(max), tag=tag) * sign(max); }

fair <- function(N, pref="tw", mul=6, runif=runif_def, tag="abcd") { 
	sprintf("%s%d", pref, floor(runif(N, tag=tag) * mul) );  }

ddm <- function(end_date, start_date) {
    ed <- as.POSIXlt(end_date);
    sd <- as.POSIXlt(start_date)
    12 * (ed$year - sd$year) + (ed$mon - sd$mon);
}

rlist_gantt <- function(N=123, wd="2023-02-15") {
  gdf <- data.frame( code=rcode(N) );
  gdf$first <- rdate(N, wd, max=-2.5*365);
  gdf$last <- rdate(N, wd, max=3.7*365);
  gdf$tile <- fair(N);
  gdf$monthly <- runif(N, min=700, max=1500);
  gdf$sqft <- floor(gdf$monthly / runif(N, min=8.5, max=9.3)); 
  gdf$pad <- gdf$first - min(gdf$first);
  gdf$len <- gdf$last - gdf$first;
  return(gdf); 
}

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

WALE_title <- function(tdf) {
   sprintf("%s: WALE = %0.1f months", max(tdf$tile), mean(ddm(tdf$last_WL, tdf$first_WL)) );
}

gantt_chart <- function(ddd, wd=as.Date("2024-03-15"), dash_col='blue', dash_type="dashed", 
	full_col='yellow', full_size=3.5, wale_col='black', wale_size=0.5) {
      tdf <- gantt_after(ddd, wd);

	ggplot(ddd) + ggtitle( WALE_title(tdf) ) + 
      geom_segment(aes(x=first, xend=last, y=code, yend=code), color=full_col, size=full_size, show.legend=FALSE) +
      geom_segment(data=tdf, aes(x=first_WL, xend=last_WL, y=code, yend=code), color=wale_col, size=wale_size, show.legend=FALSE) +
      geom_vline(aes(xintercept=wd), color=dash_col, linetype=dash_type) +
   	theme(axis.title.x = element_blank(), axis.title.y = element_blank() );
}

gantt_array <- function(gdf, ncol=3, FUN=gantt_chart) {
   ldx <- lapply(split(gdf, gdf$tile), FUN=FUN);
   grid.arrange(grobs=ldx, ncol=ncol);
}

main <- function() {
  gdf <- rlist_gantt();
  write.csv(gdf, file=user_file(".tabs/rlist-gantt.csv"), row.names=FALSE);
  print(head(gdf));
  gantt_array(gdf);
}

if(!INSIDE_PBI) main();
