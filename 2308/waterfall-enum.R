
waterfall_enum <- function(wdf, pos='bottom', lab='major', size=0.45, ncol=2) { 
    ldf <- split(wdf, wdf$tile);
    ldx <- list();

    for(nk in names(ldf)) {
        df <- waterfall_data(ldf[[nk]], lab=lab, size=size);
        ldx[[nk]] <- waterfall_chart(df, pos=pos) + facet_wrap(ncol=1, tile ~ .);
    }

    grid.arrange(grobs=ldx, ncol=ncol);
}
