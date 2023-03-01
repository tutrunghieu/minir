#source(file.path(Sys.getenv('USERPROFILE'), 'Desktop/powpods.R'));
#export_png(path=make_file('test1.png'), expr=draw_tiled_chart_xyb, dual=TRUE);

#-------------------------------------------
library(ggplot2); 
library(gridExtra);

#-------------------------------------------
add_topping_table <- function(g, df=dataset, top=11) {
    tt <- tableGrob(head(df, top));
    g <- g + annotation_custom(tt, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
    return(g);
}


#-------------------------------------------
draw_nothing <- function(df){
    ggplot(df) + facet_wrap(bridge ~ .);
}

#-------------------------------------------
draw_bridges <- function(df=dataset, cols=c("FY", "amt", "type", "tag", "bridge"), ncol=1, edit=NULL, draw=draw_nothing) {
    names(df) <- cols;
    if(! is.null(edit) ) df <- edit(df);
    grobs <- lapply(split(df, df$bridge), FUN=draw)
    grid.arrange(grobs=grobs, ncol=ncol);
}

#-------------------------------------------
fmt_c1 <- function(x, div=1) { format(round(x/div, 1), nsmall=1, big.mark=","); }

#-------------------------------------------
print11 <- function(df=dataset, top=11) { print(ggplot(df)); grid.table(head(df, top)); }


#-------------------------------------------
make_file <- function(name) {
    mk <- file.path(Sys.getenv('USERPROFILE'), 'Desktop/powpods', name);
    dir.create(dirname(mk), recursive = TRUE);
    return(mk);
}


#-------------------------------------------
draw_tiled_chart_xyb <- function(df=dataset, cols=c("panel", "FY", "amt"), fmt=fmt_c1, edit=NULL) {
    names(df) <- cols;
    if(! is.null(edit) ) df <- edit(df);

    g <- ggplot(df) + facet_wrap(scales="free", ncol=3, panel ~ .);
    g <- g + geom_bar(aes(x=FY, y=amt, fill=panel), stat="identity", show.legend=FALSE);
    g <- g + geom_text(aes(x=FY, y=amt, label=fmt(amt)), show.legend=FALSE);

    g <- g + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(angle=25) );
    print(g);
}

#-------------------------------------------
export_png <- function(path=NULL, expr=NULL, wd=640, hg=480, dual=FALSE) {
    if(dual) expr();
    if(is.null(path)) path <- file.path(Sys.getenv('USERPROFILE'), 'Desktop/out1.png');
    png(file=path, width=wd, height=hg);
    if( !is.null(expr) ) expr();
    muted <- dev.off();
    return(path);  
}

