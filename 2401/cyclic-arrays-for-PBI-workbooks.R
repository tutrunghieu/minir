library(ggplot2); library(gridExtra);

fmt_100 <- function(x, div=100) { format(round(x*div, 1), nsmall=1, big.mark=","); }
fmt_c1 <- function(x, div=1) { format(round(x/div, 1), nsmall=1, big.mark=","); }
fmt_c1_e3 <- function(x, div=1e3) { format(round(x/div, 1), nsmall=1, big.mark=","); }
scale_cy_c1_e3 <- function() { scale_y_continuous(labels=fmt_c1_e3); }
scale_cy_100 <- function() { scale_y_continuous(labels=fmt_100); }

draw_bars <- function(gdf) {  ggplot(gdf) + 
    geom_bar(aes(x=FY, y=amt, fill=name), stat="identity", show.legend=FALSE) + scale_cy_c1_e3(); }

draw_points <- function(gdf) {  ggplot(gdf) + scale_cy_100() + 
    geom_line(aes(x=FY, y=amt, group=name), show.legend=FALSE) + geom_point(aes(x=FY, y=amt, color=name), show.legend=FALSE); }

cyclic_chart <- function(gdf) {
    nk <- gdf$name[1];
    if(nk == "GR" | nk == "CT") { g <- draw_points(gdf); } else { g <- draw_bars(gdf); }
    return( g + facet_wrap(ncol=1, tile ~ .) + theme(axis.title.x = element_blank(), axis.title.y = element_blank() ) );
}

cyclic_array <- function(gdf=dataset, tile_ncol=5) {
    names(gdf) <- c("FY", "amt", "group", "name");
    gdf$tile <- paste(gdf$group, gdf$name);

    ldf <- split(gdf, gdf$tile);
    ldf <- lapply(ldf, FUN=cyclic_chart);
    grid.arrange(grobs=ldf, ncol=tile_ncol);
}

cyclic_array(); 
