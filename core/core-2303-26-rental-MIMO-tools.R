A1_stack_array <- function() { 
	draw_bar_array_YTD(); 
} 
A2_full_array <- function() { 
	draw_bar_array_YTD(make_chart=draw_full_bar_chart_YTD, show_labels=TRUE, label_fmt=fmt_100b, yy_fmt=fmt_100b); 
} 
A3_side_array <- function() { 
	draw_bar_array_YTD(make_chart=draw_side_chart_YTD); 
} 
draw_bar_array_YTD <- function(df=rename(dataset, "xx", "yy", "fill", "panel"),  
make_chart=draw_bar_chart_YTD, legend=TRUE, 
color_mapping=NULL, show_totals=TRUE, show_labels=FALSE, label_fmt=fmt_c1_e3, total_fmt=fmt_c1, yy_fmt=fmt_c1_e3, xx_angle=75, xx_complete=TRUE, xx_vline=TRUE, 
array_ncol=2, array_caption='xx', array_title='bar array', show_checksum=TRUE, 
more=NULL) { 
    if( is.null(more) ) {  
        more <- list(legend=legend, make_chart=make_chart, xx_vline=xx_vline, label_fmt=label_fmt, show_labels=show_labels, show_totals=show_totals, 
	        xx_complete=xx_complete, xx_angle=xx_angle, yy_fmt=yy_fmt, color_mapping=color_mapping); 
        more <- c(more, show_checksum=show_checksum, total_fmt=total_fmt,  
        	array_title=array_title, array_ncol=array_ncol, array_caption=array_caption); 
    } 
    more$xx_labels <- make_vocab(df$xx); 
    more$xx_cutoff <- which('LTM22'==more$xx_labels$name) + 0.5; 
    more$yy_total <- more$total_fmt( sum(df$yy) ); 
    ldf <- lapply(split(df, df$panel), FUN=function(ddd) { more$make_chart(df=ddd, more=more); }); 
     
    g <- ggplot_arrange(grobs=ldf, ncol=more$array_ncol) + theme_void(); 
     
    if( !is.null(more$array_title) ) g <- g + ggtitle(more$array_title); 
     
    tt <- paste("source:", more$array_caption); 
    if( more$show_checksum ) { tt <- paste(tt, "total:",  more$yy_total); } 
    g <- g + labs(caption=tt); 
     
    print(g); 
} 
draw_bar_chart_YTD <- function(df, more) {  
    g <- ggplot(df) + ggtitle(df$panel[1]);  
    if(more$xx_complete) { g <- g + geom_text(data=more$xx_labels, aes(x=name, y=0, label=''), show.legend=FALSE); } 
    if( more$xx_vline) { g <- g + geom_vline(aes(xintercept=more$xx_cutoff), linetype="dashed", color='blue'); } 
     
    g <- g + geom_bar(aes(x=xx, y=yy, fill=fill), stat="identity", show.legend=more$legend);        
     
    if(more$show_totals) { 
    	tdf <- annual_totals(df, thres=1e-3); 
    	g <- g + geom_text(data=tdf, aes(x=xx, y=yy, label=more$label_fmt(yy)), show.legend=FALSE);        
	} 
    g <- g + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(angle=more$xx_angle) ); 
    if( !is.null(more$color_mapping) ) { g <- g + scale_fill_manual(values=more$color_mapping); } 
    g <- g + scale_y_continuous(labels=more$yy_fmt); 
    return(g); 
} 
draw_side_chart_YTD <- function(df, more) {  
    g <- ggplot(df) + ggtitle(df$panel[1]);  
    if(more$xx_complete) { g <- g + geom_text(data=more$xx_labels, aes(x=name, y=0, label=''), show.legend=FALSE); } 
    if( more$xx_vline) { g <- g + geom_vline(aes(xintercept=more$xx_cutoff), linetype="dashed", color='blue'); } 
     
    g <- g + geom_bar(aes(x=xx, y=yy, fill=fill), stat="identity", position=position_dodge(), show.legend=more$legend);         
    if(more$show_labels) g <- g + geom_text(aes(x=xx, y=yy, label=more$label_fmt(yy), group=fill), position=position_dodge(width=0.5), show.legend=FALSE)  
        
    g <- g + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(angle=more$xx_angle) ); 
    if( !is.null(more$color_mapping) ) { g <- g + scale_fill_manual(values=more$color_mapping); } 
    g <- g + scale_y_continuous(labels=more$yy_fmt); 
    return(g); 
} 
draw_full_bar_chart_YTD <- function(df, more) {  
    tdf <- annual_totals(df); 
    for(k in 1:nrow(df)) { xk <- as.character(df$xx[k]); df[k, "yy01"] <- df$yy[k] / tdf[xk, "yy"]; } 
     
    g <- ggplot(df) + ggtitle(df$panel[1]);  
    if(more$xx_complete) { g <- g + geom_text(data=more$xx_labels, aes(x=name, y=0, label=''), show.legend=FALSE); } 
    if( more$xx_vline) { g <- g + geom_vline(aes(xintercept=more$xx_cutoff), linetype="dashed", color='blue'); } 
     
     
    g <- g + geom_col(aes(x=xx, y=yy, fill=fill), stat="identity", position="fill", show.legend=FALSE);         
    if(more$show_labels) g <- g + geom_text(aes(x=xx, y=yy, label=more$label_fmt(yy01), group=fill), position=position_fill(vjust=0.5), show.legend=FALSE) + ggtitle(df$panel[1]);          
    g <- g + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(angle=more$xx_angle) ); 
    if( !is.null(more$color_mapping) ) { g <- g + scale_fill_manual(values=more$color_mapping); } 
    g <- g + scale_y_continuous(labels=more$yy_fmt); 
    return(g); 
} 
annual_totals <- function(df, thres=NULL) { 
    ldf <- sapply(split(df, df$xx), FUN=function(ddd) { sum(ddd$yy) }); 
    df <- data.frame(xx=names(ldf), yy=ldf); 
    row.names(df) <- names(ldf); 
     
    if( !is.null(thres) ) df <- df[abs(df$yy)>thres, ]; 
         
    return(df); 
} 
add_topping_totals_123 <- function(g, df, more) { 
    ldf <- sapply(split(df, df$xx), FUN=function(ddd) { sum(ddd$yy) }); 
    df <- data.frame(xx=names(ldf), yy=ldf); 
    df <- df[abs(df$yy)>0.5, ]; 
    g <- g + geom_text(data=df, aes(x=xx, y=yy, label=more$label_fmt(yy)), show.legend=FALSE);        
    return(g); 
} 
A4_bridge_array <- function() { 
	draw_bridge_array_YTD(); 
} 
draw_bridge_array_YTD <- function(df=rename(dataset, "xx", "yy", "fill", "tag", "panel"), more=NULL, make_chart=draw_bridge_YTD, 
color_mapping=NULL, xx_complete=TRUE, xx_vline=TRUE, box_width=0.47, yy_fmt=fmt_c1_e3, fmt=fmt_c1_e3, xx_angle=25,  
major_mult=0.5, major_angle=0, minor_angle=25, legend=TRUE, show_totals=TRUE, total_fmt=fmt_c1, 
array_ncol=1, array_caption='xx', array_title='bar array', show_checksum=TRUE) { 
    if( is.null(more) ) {  
        more <- list(make_chart=make_chart, xx_vline=xx_vline, show_totals=show_totals, 
	        xx_complete=xx_complete, xx_angle=xx_angle, yy_fmt=yy_fmt, color_mapping=color_mapping); 
        more <- c(more, show_checksum=show_checksum, total_fmt=total_fmt,  
        	array_title=array_title, array_ncol=array_ncol, array_caption=array_caption); 
    } 
    more$xx_labels <- make_vocab(df$xx); 
    more$xx_cutoff <- which('YTD21'==more$xx_labels$name) - 0.5; 
    more$yy_total <- more$total_fmt( sum(df$yy) ); 
    ldf <- lapply(split(df, df$panel), FUN=function(ddd) {  
    	more$make_chart(df=ddd, more=more);  
    }); 
     
    g <- ggplot_arrange(grobs=ldf, ncol=more$array_ncol) + theme_void(); 
     
    if( !is.null(more$array_title) ) g <- g + ggtitle(more$array_title); 
     
    tt <- paste("source:", more$array_caption); 
    if( more$show_checksum ) { tt <- paste(tt, "total:",  more$yy_total); } 
    g <- g + labs(caption=tt); 
     
    print(g); 
} 
draw_bridge_YTD <- function(df=rename(dataset, "xx", "yy", "fill", "tag", "panel"),   
color_mapping=list(major='#ffe600', hypermarkets='#797991', supermarkets='#d2d2da', others='#9c82d4'),  
xx_complete=TRUE, xx_vline=TRUE, box_width=0.47, yy_fmt=fmt_c1_e3, fmt=fmt_c1_e3, xx_angle=25,  
major_mult=0.5, major_angle=0, minor_angle=25, legend=TRUE, 
more=NULL) { 
    g <- ggplot(df); 
     
    if(more$xx_complete) { g <- g + geom_text(data=more$xx_labels, aes(x=name, y=0, label=''), show.legend=FALSE); } 
    if( more$xx_vline) { g <- g + geom_vline(aes(xintercept=more$xx_cutoff), linetype="dashed", color='blue'); } 
    df1 <- df[df$tag=="major", ];     
    g <- g + geom_bar(data=df1, aes(x=xx, y=yy, fill=fill), stat="identity", show.legend=more$legend); 
    g <- g + geom_text(data=df1, aes(x=xx, y=yy*major_mult, label=fmt(yy) ), angle=major_angle, show.legend=FALSE); 
    s <- 0; 
    for(k in 1:nrow(df)) { if(df$tag[k] == "major") { s <- df$yy[k]; } else { df[k, "y1"] <- s; s <- s + df$yy[k]; df[k, "y2"] <- s; } } 
    df$x1 <- as.integer(df$xx) - box_width; 
    df$x2 <- df$x1 + 2*box_width; 
    df1 <- df[df$tag!="major", ];     
    g <- g + geom_rect(data=df1, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=fill), show.legend=legend); 
    g <- g + geom_text(data=df1, aes(x=(x1+x2)/2, y=(y1+y2)/2, label=fmt(yy) ), angle=minor_angle, show.legend=FALSE); 
    g <- g + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(angle=xx_angle) ); 
    g <- g + scale_fill_manual(values=more$color_mapping); 
    g <- g + scale_y_continuous(labels=yy_fmt); 
    print(g); 
} 
library(ggplot2); library(openssl); library(gridExtra); 
factor_JanDec <- function(vals) { 
	JanDec <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Q1", "Q2", "Q3", "Q4"); 
	vals <- factor(vals, levels=JanDec); 
	return(vals);  
} 
make_output_file <- function(name='test1.txt') { 
    path <- file.path(Sys.getenv('USERPROFILE'), 'Desktop/out1', name); 
    dir.create(dirname(path), recursive=TRUE, showWarnings = FALSE); 
    return(path); 
} 
export_png <- function(expr, wd=PANEL_WIDTH, hg=PANEL_HEIGHT, path=NULL, dual=FALSE) { 
    if(dual) expr(); 
    if(is.null(path)) path <- file.path(Sys.getenv('USERPROFILE'), 'Desktop/out1.png'); 
    dir.create(dirname(path), recursive=TRUE, showWarnings = FALSE); 
    png(file=path, width=wd, height=hg); 
    if( !is.null(expr) ) expr(); 
    muted <- dev.off(); 
    return(path);   
} 
ggcaption_if <- function(tt) { 
	if( is.null(tt) ) return(NULL); 
	return(labs(caption=tt)); 
} 
ggtitle_if <- function(tt) { 
	if( is.null(tt) ) return(NULL); 
	return(ggtitle(tt)); 
} 
gt11 <- function(df=dataset, top=11) {  
	grid.table( head(df, top) );  
} 
hex_to_int <- function(s, pref='0x') { strtoi(paste0(pref, s)); } 
unique_vals <- function(...) { 
    vals <- as.character(unlist(list(...))); 
	voc <- c(); 
	for(vk in vals) voc[vk] <- 1;  
	voc <- names(voc); 
	 
	return( voc[order(voc)] ); 
} 
unique_values <- function(vals=NULL, dec=TRUE) { 
    vals <- as.character(unlist(vals)); 
	voc <- c(); 
	for(vk in vals) voc[vk] <- 1;  
	voc <- names(voc); 
	 
	sel <- order(voc, decreasing=dec); 
	return(voc[sel]); 
} 
factor0 <- function(vals, voc=NULL) { 
    vals <- as.character(vals); 
    if( is.null(voc) ) voc <- vals; 
	factor(vals, levels=voc); 
} 
main <- function(name='figure1', expr=NULL) {  
	expr();  
} 
rename <- function(df, ...) {  
	names(df) <- unlist(list(...));  
	return(df);  
} 
make_color <- function(nk, more=NULL, pos=2) {  
	paste0('#', substr(md5(nk), pos, pos+5) );  
} 
make_color_dual <- function(nk, more=NULL, pos=2) {  
	ck <- more$color_mapping[[nk]]; 
	if( is.null(ck) ) ck <- paste0('#', substr(md5(nk), pos, pos+5) );  
	return(ck); 
} 
make_color_HSO <- function(nk, map=NULL, pos=2) {  
   map <- list(hypermarkets='#ffe600', supermarkets='#797991', others='#d2d2da'); 
   return(map[[nk]]);	 
} 
make_vocab <- function(vals, cols=c("name", "freq")) { 
    df <- data.frame(table(vals)); 
    names(df) <- cols; 
    return(df); 
} 
fmt_z <- function(x) { x } 
fmt_100 <- function(x, s=100, def='') { 
	if( is.na(x) ) return(def);  
	paste0(format(round(x*s, 1), nsmall=1, big.mark=","), '%'); 
} 
fmt_100b <- function(x, s=100, def='') {  
	if( is.na(x) ) return(def);  
	format(round(x*s, 1), nsmall=1, big.mark=",");  
} 
fmt_c1 <- function(x, div=1, def='') {  
	if( is.na(x) ) return(def);  
	format(round(x/div, 1), nsmall=1, big.mark=","); } 
	 
fmt_c1_e3 <- function(x, div=1e3, def='') {  
	if( is.na(x) ) return(def);  
	format(round(x/div, 1), nsmall=1, big.mark=","); } 
fmt_c1_e6 <- function(x, div=1e6, def='') { 
	if( is.na(x) ) return(def);  
	format(round(x/div, 1), nsmall=1, big.mark=","); } 
	 
fmt_c1_e9 <- function(x, div=1e9, def='') { 
	if( is.na(x) ) return(def); 
	format(round(x/div, 1), nsmall=1, big.mark=","); } 
draw_carousel_legend <- function(df=rename(dataset, "xx", "yy", "fill"), more=NULL) {  
    if( is.null(more) ) { 
        more <- list(legend_pos="right"); 
    	more$color_mapping <- list('A, top 10 customers'='yellow', 'B, other customers'='grey', 'A, top 10 stores'='yellow', 'B, other stores'='grey') 
    } 
    g <- ggplot(df) + geom_bar(aes(x=xx, y=0, fill=fill), stat="identity", show.legend=TRUE);  
    g <- g + theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank() ); 
    g <- g + theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank() ); 
    g <- g + theme(legend.position=more$legend_pos) + labs(fill=''); 
    if( !is.null(more$color_mapping) ) g <- g + scale_fill_manual(values=more$color_mapping); 
    return(g); 
} 
draw_carousel_chart1 <- function(ddd, more) {  
    g <- ggplot(ddd) + geom_bar(aes(x=xx, y=yy, fill=fill), stat="identity", show.legend=more$legend);  
    g <- g + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(angle=more$xx_angle) ); 
    g <- g + theme(legend.position="bottom") + labs(fill=''); 
    g <- g + scale_y_continuous(labels=more$yy_fmt); 
    if( !is.null(more$color_mapping) ) g <- g + scale_fill_manual(values=more$color_mapping); 
    return(g); 
} 
draw_carousel_chart2 <- function(ddd, more) {   
    g <- ggplot(ddd) + geom_bar(aes(x=xx, y=yy, fill=fill), stat="identity", position="fill", show.legend=more$legend);  
    g <- g + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(angle=more$xx_angle) ); 
    g <- g + theme(legend.position="bottom") + labs(fill=''); 
    if( !is.null(more$color_mapping) ) g <- g + scale_fill_manual(values=more$color_mapping); 
    return(g); 
} 
draw_carousel_SF <- function(df=rename(dataset, "xx", "yy", "fill"), more=NULL) { 
    if( is.null(more) ) { 
        more <- list(chart1=draw_carousel_chart1, chart2=draw_carousel_chart2, array_ncol=2, xx_angle=15, yy_fmt=fmt_c1_e3, legend=FALSE); 
	more$color_mapping <- list('A, top 10 customers'='yellow', 'B, other customers'='grey', 'A, top 10 stores'='yellow', 'B, other stores'='grey') 
    } 
     
    ldf <- list(stack=more$chart1(df, more), full=more$chart2(df, more)); 
    grid.arrange(grobs=ldf, ncol=more$array_ncol); 
} 
A5_line_array <- function() { 
	draw_line_array_YTD(legend=FALSE, xx_angle=25); 
} 
draw_line_array_YTD <- function(df=rename(dataset, "xx", "yy", "fill", "panel"),  
color_mapping=NULL, yy_fmt=fmt_c1_e3, label_fmt=fmt_c1_e3, xx_angle=0, make_col=make_color_dual, legend=TRUE, xx_complete=TRUE, xx_vline=TRUE, 
array_ncol=2, array_title='line array', array_caption='xx', show_checksum=TRUE, fmt_checksum=fmt_c1, 
more=NULL) { 
    if( is.null(more) ) {  
        more <- list(color_mapping=color_mapping, yy_fmt=yy_fmt, label_fmt=label_fmt, xx_angle=xx_angle, make_col=make_col, xx_complete=xx_complete, xx_vline=xx_vline, legend=legend); 
        more <- c(more, show_checksum=show_checksum, fmt_checksum=fmt_checksum, array_ncol=array_ncol, array_title=array_title, array_caption=array_caption); 
    } 
     
    more$xx_labels <- make_vocab(df$xx); 
    more$yy_total <- more$fmt_checksum(sum(df$yy)); 
    more$xx_cutoff <- which('LTM22'==more$xx_labels$name) + 0.5; 
    ldf <- lapply(split(df, df$panel), FUN=function(ddd) { draw_lines_YTD(df=ddd, more=more) + ggtitle(df$panel[1]); });         
    g <- ggplot_arrange(grobs=ldf, ncol=more$array_ncol) + theme_void(); 
     
    if( !is.null(more$array_title) ) g <- g + ggtitle(more$array_title); 
     
    tt <- paste("source:", more$array_caption); 
    if( more$show_checksum ) { tt <- paste(tt, "total:",  more$yy_total); } 
    g <- g + labs(caption=tt); 
     
    print(g); 
} 
draw_lines_YTD <- function(df=rename(dataset, "xx", "yy", "fill"), 
yy_fmt=fmt_c1_e3, label_fmt=fmt_c1_e3, xx_angle=0, make_col=make_color_HSO, legend=TRUE, xx_complete=TRUE, xx_vline=TRUE, 
more=NULL) { 
    if( is.null(more) ) {  
        more <- list(yy_fmt=yy_fmt, xx_complete=xx_complete, xx_vline=xx_vline, label_fmt=label_fmt, xx_angle=xx_angle, make_col=make_col, legend=legend); 
    } 
    g <- ggplot(df); 
    if(more$xx_complete) { g <- g + geom_text(data=more$xx_labels, aes(x=name, y=0, label=''), show.legend=FALSE); } 
    if( more$xx_vline) { g <- g + geom_vline(aes(xintercept=more$xx_cutoff), linetype="dashed", color='blue'); } 
     
    df$sel <- as.character(df$xx); 
    g <- add_lines_YTD(g=g, df=df[df$sel<"YTD", ], more=more); 
    g <- add_lines_YTD(g=g, df=df[df$sel>"YTD", ], more=more); 
    print(g); 
} 
add_lines_YTD <- function(g=NULL, df=NULL, 
yy_fmt=fmt_c1_e3, label_fmt=fmt_c1_e3, xx_angle=0, make_col=make_color_HSO, legend=TRUE, 
more=NULL) { 
    if( is.null(more) ) {  
        more <- list(yy_fmt=yy_fmt, label_fmt=label_fmt, xx_angle=xx_angle, make_col=make_col, legend=legend); 
    } 
     
    map <- list();     
    ldf <- split(df, df$fill); 
    for(nk in names(ldf)) { 
        ddd <- ldf[[nk]]; 
        if(nrow(ddd)==0) next; 
        ck <- more$make_col(nk, more); 
        map[[nk]] <- ck; 
        g <- g + geom_rect(data=ddd, aes(xmin=xx, xmax=xx, ymin=yy, ymax=yy, fill=fill), show.legend=more$legend); 
        g <- g + geom_line(data=ddd, aes(x=xx, y=yy, fill=fill, group=fill), color=ck, show.legend=FALSE); 
        g <- g + geom_point(data=ddd, aes(x=xx, y=yy, fill=fill, group=fill), color=ck, show.legend=FALSE); 
        g <- g + geom_text(data=ddd, aes(x=xx, y=yy, label=more$label_fmt(yy), group=fill), show.legend=FALSE); 
    } 
    g <- g + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(angle=more$xx_angle) ); 
    g <- g + scale_y_continuous(labels=more$yy_fmt); 
    g <- g + scale_fill_manual(values=map); 
    return(g); 
} 
A6_pie_array <- function() { 
	draw_pie_array(); 
} 
draw_pie_array <- function(df=rename(dataset, "xx", "yy", "panel"), 
color_mapping=NULL, yy_fmt=fmt_c1_e3, legend=TRUE, 
array_ncol=2, array_title='pie array', array_caption='xx', show_checksum=TRUE, fmt_checksum=fmt_c1, 
more=NULL) { 
    if( is.null(more) ) {  
        more <- list(color_mapping=color_mapping, yy_fmt=yy_fmt, legend=legend); 
        more <- c(more, show_checksum=show_checksum, fmt_checksum=fmt_checksum,  
        	array_title=array_title, array_ncol=array_ncol, array_caption=array_caption); 
    } 
	 
    more$yy_total <- more$fmt_checksum( sum(df$yy) ); 
    ldf <- lapply(split(df, df$panel), FUN=function(ddd) { draw_pie_chart(df=ddd, more=more) + ggtitle(ddd$panel[1]); }); 
    g <- ggplot_arrange(grobs=ldf, ncol=more$array_ncol) + theme_void(); 
     
    if( !is.null(more$array_title) ) g <- g + ggtitle(more$array_title); 
     
    tt <- paste("source:", more$array_caption); 
    if( more$show_checksum ) { tt <- paste(tt, "total:",  more$yy_total); } 
    g <- g + labs(caption=tt); 
     
    print(g); 
}     
draw_pie_chart <- function(df=rename(dataset, "xx", "yy", "panel"),  
yy_fmt=fmt_c1_e3, color_mapping=NULL, legend=FALSE, 
more=NULL) { 
    if( is.null(more) ) {  
        more <- list(color_mapping=color_mapping, yy_fmt=yy_fmt, legend=legend); 
        more <- c(more, show_checksum=show_checksum, fmt_checksum=fmt_checksum,  
        	array_title=array_title, array_ncol=array_ncol, array_caption=array_caption); 
    } 
     
    g <- ggplot(df) + geom_bar(aes(x="", y=yy, fill=xx), stat="identity", width=1, show.legend=more$legend) + coord_polar("y", start=0) + theme_void(); 
    g <- g + geom_text(aes(x="", y=yy, label = more$yy_fmt(yy), fill=xx ), position = position_stack(vjust = 0.5)); 
    g <- g + scale_fill_manual(values=more$color_mapping); 
    return(g); 
} 
sort_by_XXL <- function(vals) { 
    factor(vals, levels=c("XXS", "XS", "S", "M", "L", "XL", "XXL")); 
} 
sort_XXL_AC <- function(vals) { 
    factor(vals, levels=c("XXS/AC", "XXS/NAC", "XS/AC", "XS/NAC", "S/AC", "S/NAC",  
    "M/AC", "M/NAC", "L/AC", "L/NAC", "XL/AC", "XL/NAC", "XXL/AC", "XXL/NAC")) 
} 
draw_boxplot_array <- function(df=rename(dataset, "xx", "yy", "fill"), xx_angle=25, legend=TRUE, sort_xx=sort_by_XXL, more=NULL) { 
    if( is.null(more) ) {  
        more <- list(sort_xx=sort_xx, xx_angle=xx_angle, xx_angle=xx_angle, legend=legend); 
    } 
     
    df$xx <- sort_xx(df$xx); 
    g <- ggplot(df) + geom_jitter(aes(x=xx, y=yy, color=xx), show.legend=more$legend); 
    g <- g + geom_boxplot(aes(x=xx, y=yy), color='black', fill=NA, show.legend=more$legend); 
    g <- g + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(angle=more$xx_angle) ); 
    print(g); 
} 
draw_lifespan_chart <- function(df=rename(dataset, "xx", "yy", "DMI", "DMO", "fill", "panel"), 
xx_angle=25, legend=TRUE, label_angle=25, bar_width=0.45, 
more=NULL) { 
    if( is.null(more) ) {  
        more <- list(xx_angle=xx_angle, xx_angle=xx_angle, bar_width=bar_width, label_angle=label_angle, legend=legend); 
    } 
     
    dates <- unique_vals(df$DMI, df$DMO); 
    df$x1 <- as.integer( factor0(vals=df$DMI, voc=dates) ); 
    df$x2 <- as.integer( factor0(vals=df$DMO, voc=dates) ); 
    codes <- unique_vals(df$xx); 
    df$y1 <- as.integer( factor0(vals=df$xx, voc=codes) ) - bar_width; 
    df$y2 <- df$y1 + 2 * bar_width;     
    g <- ggplot(df); 
    g <- g + geom_text(aes(x=DMI, y=xx, label=''), show.legend=FALSE); 
    g <- g + geom_text(aes(x=DMO, y=xx, label=''), show.legend=FALSE); 
    g <- g + geom_rect(aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=fill), show.legend=FALSE); 
    g <- g + geom_text(aes(x=DMI, y=xx, label=DMI), angle=more$label_angle, show.legend=FALSE); 
    g <- g + geom_text(aes(x=DMO, y=xx, label=DMO), angle=more$label_angle, show.legend=FALSE); 
     
    g <- g + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(angle=more$xx_angle) ); 
    print(g); 
} 
draw_lifespan_array <- function(df=rename(dataset, "xx", "yy", "DMI", "DMO", "fill", "panel"), 
xx_angle=25, legend=TRUE, label_angle=25, bar_width=0.45, 
array_ncol=2, array_title='lifespan array', array_caption='xx', show_checksum=TRUE, fmt_checksum=fmt_c1, 
more=NULL) { 
    if( is.null(more) ) {  
        more <- list(xx_angle=xx_angle, xx_angle=xx_angle, bar_width=bar_width, label_angle=label_angle, legend=legend); 
        more <- c(more, array_ncol=array_ncol, array_title=array_title, array_caption=array_caption, fmt_checksum=fmt_checksum, show_checksum=show_checksum); 
    } 
    more$yy_total = more$fmt_checksum( sum(df$yy) ); 
    ldf <- lapply(split(df, df$panel), FUN=function(ddd) { draw_lifespan_chart(df=ddd, more=more) + ggtitle(ddd$panel[1]); }) 
    g <- ggplot_arrange(grobs=ldf, ncol=more$array_ncol) + theme_void(); 
     
    if( !is.null(more$array_title) ) g <- g + ggtitle(more$array_title); 
     
    tt <- paste("source:", more$array_caption); 
    if( more$show_checksum ) { tt <- paste(tt, "total:",  more$yy_total); } 
    g <- g + labs(caption=tt); 
     
    print(g); 
     
} 
flip_table <- function(df=dataset, fmt_def=fmt_c1,  
fmt_col=list(time_y=fmt_z, '__PUSH'=fmt_z, '__DOSH'=fmt_z),  
more=NULL) { 
    rdf <- data.frame(); 
    fmt <- list(); 
    for(cj in names(df)) { fj <- fmt_col[[cj]]; fmt[[cj]] <- ifelse(is.null(fj), fmt_def, fj); } 
    for(k in 1:nrow(df)) for(cj in names(df)) { rdf[cj, k] <- fmt[[cj]]( df[k, cj] ); }             
    grid.table(head(rdf, 11)); 
} 
draw_legend_GANTT <- function(df=rename(dataset, "xx"), color_seed="123'123", preset=list(CH1507='red', CH1508='green', CH1509='black')) { 
    g <- ggplot(df) + theme_void() + geom_bar(aes(x=xx, y=0, fill=xx), stat="identity", show.legend=TRUE); 
    col <- rand_colors(vals=unique_vals(df$xx), seed=color_seed, preset=preset); 
    g <- g + scale_fill_manual(values=col, name='') + theme(legend.position="top"); 
     
    print(g); 
} 
rand_colors <- function(vals, sk=1, ek=6, seed="abcd", preset=NULL) { 
    res <- list(); 
    for(vk in vals) {  
        ck <- preset[[vk]]; 
        if( is.null(ck) ) ck <- paste0('#', substr(md5(paste(vk, seed)), sk, ek));  
        res[[vk]] <- ck; 
    } 
     
    return(res); 
} 
toy_colors <- c('#FF0000', '#00FF00', '#0000FF', '#FF00FF', '#00FFFF', '#FFFF00'); 
base_colors <- c("#2DB757", "#27ACAA", "#188CE5", "#3D108A", "#FF4136", "#FF6D00", "#FFE600",  
"#2E2E38", "#797991", "#D2D2DA", "#95CB89", "#F04C3E"); 
G1_colors <- c("#C882B2", "#B04791", "#922B73", "#74135C", "#5A0C41", "#41152C", "#351A1F"); 
G2_colors <- c("#FF9A91", "#FF726A", "#F95C53", "#FF4135", "#DF352B", "#B8261F", "#791511"); 
G3_colors <- c("#FFB46A", "#FF972F", "#FF8117", "#FF6D15", "#F86915", "#EB4F13", "#BB2D0F"); 
G4_colors <- c("#8BE8AC", "#56E087", "#33C668", "#2BB755", "#1C9D3C", "#1A8635", "#15642A"); 
G5_colors <- c("#91F0E7", "#5FE7E0", "#43C8C2", "#25ACAA", "#199090", "#157575", "#064F4E"); 
G6_colors <- c("#86D2F2", "#4EBDEB", "#35A4E8", "#1C8CE4", "#1D77CF", "#1A5BB4", "#092B63"); 
G7_colors <- c("#9C82D4", "#704AC3", "#532BA4", "#3C1788", "#221175", "#09095A", "#15153C"); 
draw_legend_77 <- function(mode="") { 
    ldf <- list(); 
    ldf$set1 <- draw_floating_legend(df=data.frame(name=G1_colors), make=lapply_11, ncol=NULL, mode="g"); 
    ldf$set2 <- draw_floating_legend(df=data.frame(name=G2_colors), make=lapply_11, ncol=NULL, mode="g"); 
    ldf$set3 <- draw_floating_legend(df=data.frame(name=G3_colors), make=lapply_11, ncol=NULL, mode="g"); 
    ldf$set4 <- draw_floating_legend(df=data.frame(name=G4_colors), make=lapply_11, ncol=NULL, mode="g"); 
    ldf$set5 <- draw_floating_legend(df=data.frame(name=G5_colors), make=lapply_11, ncol=NULL, mode="g"); 
    ldf$set6 <- draw_floating_legend(df=data.frame(name=G6_colors), make=lapply_11, ncol=NULL, mode="g"); 
    ldf$set7 <- draw_floating_legend(df=data.frame(name=G7_colors), make=lapply_11, ncol=NULL, mode="g"); 
    if(mode == "g") return( arrangeGrob(grobs=ldf, ncol=1) ); 
    grid.arrange(grobs=ldf, ncol=1); 
}     
make_rainbow_colors <- function(n=9) { 
    cols <- rainbow(n); 
    return(lapply_11(cols)); 
} 
lapply_11 <- function(vals, sk=1, ek=6, seed="123'123", preset=NULL) { 
	vals <- as.character(vals); 
	names(vals) <- vals; 
	return(vals); 
} 
lapply_1x <- function(vals, preset=toy_colors) { 
	vals <- as.character(vals); 
	 
	ldf <- list();	k <- 0; nn <- length(preset); 
	for(vk in vals) { ldf[[vk]] <- preset[[k+1]]; k <- (k + 1) %% nn; }  
		 
	return(ldf); 
} 
lapply_md516 <- function(vals, sk=1, ek=6, seed="123'123", preset=NULL) { 
    ldf <- list(); 
    for(nk in vals) { 
    	ck <- preset[[nk]]; 
	    if( is.null(ck) ) ck <- paste0('#', substr(md5(paste(nk, seed)), sk, ek)); 
        ldf[[nk]] <- ck; 
    }; 
    return(ldf); 
} 
draw_floating_legend <- function(df=rename(dataset, "name"), make=lapply_md516, preset=NULL, mode="", ncol=5) {     
	if( is.data.frame(df) ) { vals <- as.character(df$name); } else { vals <- as.character(df); } 
    color_mapping <- make(vals=vals, preset=preset); 
    ldf <- list(); 
    for(nk in names(color_mapping) ) { 
        ck <- color_mapping[[nk]]; 
        ldf[[ck]] <- ggplot() + theme_void() + ggtitle(nk) +  
            geom_rect(aes(xmin=0, ymin=0, xmax=1, ymax=1), fill=ck, show.legend=FALSE); 
    } 
     
    if( is.null(ncol) ) ncol <- length(ldf); 
     
    if(mode == "g") return( arrangeGrob(grobs=ldf, ncol=ncol) ); 
    grid.arrange(grobs=ldf, ncol=ncol); 
} 
draw_color_matrix <- function(cols=toy_colors, make=make_tones, mul=NULL) { 
	if( is.null(mul) ) {  
		ff <- function(x) { draw_color_scale(make(x)); }  
	} else {  
		ff <- function(x) { draw_color_scale(make_tones(col=x, mul=mul)); } 
	} 
	 
	ldf <- lapply(cols, FUN=ff); 
    grid.arrange(grobs=ldf, ncol=1); 
} 
draw_color_scale <- function(cols) {  
     
    ldf <- lapply(cols, FUN=function(x) {  
        g <- ggplot() + theme_void() + ggtitle(x) + geom_rect(aes(xmin=0, ymin=0, xmax=1, ymax=1), fill=x, show.legend=FALSE);  
    }); 
    return(arrangeGrob(grobs=ldf, ncol=length(ldf)) ); 
} 
make_tones <- function(col='#FF0000', mul=c(1, 0.75, 0.5, 0.25), show=FALSE, mode="") { 
    r1 <- hex_to_int(substr(col, 2, 3)); 
    g1 <- hex_to_int(substr(col, 4, 5)); 
    b1 <- hex_to_int(substr(col, 6, 7)); 
    df <- data.frame(); 
    for(k in seq_along(mul)) { 
        mk <- mul[[k]]; 
        df[k, "col"] <- sprintf("#%02X%02X%02X", as.integer(r1 * mk), as.integer(g1 * mk), as.integer(b1 * mk) ); 
        df[k, "mul"] <- mk; 
    } 
    if(show) grid.table(df); 
    if(mode == "df") return(df); 
    return(df$col); 
} 
test_colors <- function(cols=c('red', 'green', 'blue', 'pink', 'black', 'yellow'), years=c("FY19", "FY20", "FY21", "FY22"),  
seed=197, ncol=2) { 
    set.seed(seed); 
    df <- expand.grid(name=cols, xx=years); 
    df$yy <- runif(nrow(df)) + runif(nrow(df)); 
    df$name <- factor(df$name, levels=cols); 
     
    map <- cols; 
    names(map) <- cols; 
    ldf <- list(bars=draw_bars_GANTT(df, map), lines=draw_lines_GANTT(df, map),  
    full=draw_bars_GANTT(df, map, pos="fill"), full=draw_bars_GANTT(df, map, pos="dodge") ); 
    grid.arrange(grobs=ldf, ncol=ncol); 
} 
draw_bars_GANTT <- function(df, map, pos="stack") { 
    g <- ggplot(df); 
    g <- g + geom_bar(aes(x=xx, y=yy, fill=name), position=pos, stat="identity", show.legend=TRUE); 
    g <- g + theme(axis.title.x = element_blank(), axis.title.y = element_blank()); 
    g <- g + scale_fill_manual(values=map, name=''); 
    return(g); 
} 
draw_lines_GANTT <- function(df, map, line_size=1, dot_size=1.5) { 
    g <- ggplot(df); 
    ldf <- split(df, df$name); 
     
    for(nk in names(ldf)) { 
        ddd <- ldf[[nk]]; ck <- map[[nk]]; 
        g <- g + geom_line(data=ddd, aes(x=xx, y=yy, group=name), color=ck, size=line_size, show.legend=FALSE); 
        g <- g + geom_point(data=ddd, aes(x=xx, y=yy, group=name), color=ck, size=dot_size, show.legend=FALSE); 
        g <- g + geom_rect(data=ddd, aes(xmin=xx, ymin=yy, xmax=xx, ymax=yy, fill=name), show.legend=TRUE); 
    } 
    g <- g + theme(axis.title.x = element_blank(), axis.title.y = element_blank()); 
    g <- g + scale_fill_manual(values=map, name=''); 
    return(g); 
} 
geom_bar_xyfill <- function(pos="stack", legend=TRUE, more=NULL) { 
    geom_bar(aes(x=xx, y=yy, fill=fill), stat="identity", position=pos, show.legend=legend); 
} 
geom_place <- function(gg, more=NULL) { 
	annotation_custom(gg, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf); 
}  
geom_front <- function(gg, more=NULL) { 
	annotation_custom(gg, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf); 
}  
geom_head <- function(df, top=5, more=NULL) { 
	annotation_custom(tableGrob(head(df, top)), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf); 
}  
geom_array <- function(grobs, ncol=2, more=NULL) { 
	if( is.null(ncol) ) ncol <- length(grobs); 
	annotation_custom(arrangeGrob(grobs=grobs, ncol=ncol), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf); 
}  
ggplot_void <- function(gg) {  
	return( ggplot() + theme_void() + geom_front(gg) );  
} 
ggplot_more <- function(df=NULL, more=NULL) { 
	ggplot(); 
} 
ggplot_arrange <- function(grobs, ncol=2, g=ggplot()) { 
	gg <- arrangeGrob(grobs=grobs, ncol=ncol); 
    g <- g + annotation_custom(gg, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf); 
    return(g); 
} 
ggplot_table <- function(df, top=11, g=ggplot()) { 
	gg <- tableGrob(head(df, top)); 
    g <- g + annotation_custom(gg, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf); 
    return(g); 
} 
GLOBAL_COLORS <- list(Others='grey', major='yellow', B2B='pink', B2C='cyan', agents='magenta', 'cold storage'='blue'); 
derive_color <- function(col='red', mul=0.5) { 
    v <- as.double(col2rgb(col)) * mul; 
    v <- rgb(red=v[1], green=v[2], blue=v[3], max=255); 
    return(v); 
} 
make_md5_color <- function(nk, seed='abc', sk=1, ek=6) {  
	paste0('#', substr(md5(nk), sk, ek) );  
} 
scale_fill_grad <- function(vals, col='green', name='', pad=2, mode="") { 
    vals <- unique_vals(vals); 
    map <- list(); k <- 0; nj <- length(vals) + pad; 
    for(vk in vals) { map[[vk]] <- derive_color(col, 1-k/nj); k <- k + 1; } 
     
    for(nk in names(GLOBAL_COLORS)) { map[[nk]] <- GLOBAL_COLORS[[nk]]; } 
    if(mode == "map") return(map); 
    return( scale_fill_manual(values=map, name=name) ); 
} 
scale_fill_preset <- function(map, preset=toy_colors) { 
	for(nk in names(preset)) { map[[nk]] <- preset[[nk]]; } 
	return(map); 
} 
scale_fill_duet <- function(vals, col1='#FF0000FF', col2='#00FF00FF', mix=c(1, 0.7, 0.5), name='', mode="") { 
    vals <- unique_vals(vals); 
    map <- list(); k <- 0; nj <- length(mix); 
    for(vk in vals) { map[[vk]] <- derive_color(if( (k%%2)== 0 ) col1 else col2, mix[[ (k/2)%%nj + 1 ]]); k <- k + 1; } 
     
    for(nk in names(GLOBAL_COLORS)) { map[[nk]] <- GLOBAL_COLORS[[nk]]; } 
    if(mode == "map") return(map); 
    return( scale_fill_manual(values=map, name=name) ); 
} 
scale_fill_mod <- function(vals, cols=toy_colors, name='', mode="") { 
    vals <- unique_vals(vals); 
    map <- list(); k <- 0; nn <- length(cols); 
    for(vk in vals) { map[[vk]] <- cols[[k+1]]; k <- (k + 1) %% nn; } 
     
    for(nk in names(GLOBAL_COLORS)) { map[[nk]] <- GLOBAL_COLORS[[nk]]; } 
    if(mode == "map") return(map); 
    return( scale_fill_manual(values=map, name=name) ); 
} 
scale_fill_md5 <- function(vals, col=NULL, sk=1, ek=6, seed="123/123", name='', mode="") { 
    vals <- unique_vals(vals); 
    map <- list();  
    for(vk in vals) { map[[vk]] <- paste0('#', substr(md5(paste(vk, seed)), sk, ek)); } 
     
    for(nk in names(GLOBAL_COLORS)) { map[[nk]] <- GLOBAL_COLORS[[nk]]; } 
    if(mode == "map") return(map); 
    return( scale_fill_manual(values=map, name=name) ); 
} 
vars <- dget( file=make_output_file('project-params.R') ); 
list_stores <- vars$list_stores; 
PANEL_WIDTH <- vars$PANEL_WIDTH; 
PANEL_HEIGHT <- vars$PANEL_HEIGHT; 
draw_SBS_trends <- function() { 
	df <- rename(dataset, "xx", "yy", "fill", "panel"); 
	 
	save_as <- which(df$panel[1] == list_stores); 
	save_as = make_output_file(name = sprintf("store-%02d/page1-trends.png", save_as) ); 	 
    array_title <- sprintf('Customer trends for %s with total %s', df$panel[1], fmt_c1(sum(df$yy)), save_as); 
     
    color_mapping <- scale_fill_preset(scale_fill_md5(vals=df$fill, mode="map"),  
    	preset=list(agents='yellow') ); 
     
    more <- list(xx_angle=25, xx_size=14, legend=FALSE, color_mapping=color_mapping, 
	    array_ncol=2, array_title=array_title, array_caption=vars$source1); 
chart1 <- function(df, more=NULL) {  
	g <- ggplot(df) + ggtitle("chart A1") + geom_bar(aes(x=xx, y=yy, fill=fill), stat="identity", show.legend=more$legend); 
	g <- g + scale_fill_manual(values=more$color_mapping, name='');  
	return(fmt_chart(g)); } 
chart2 <- function(df, more=NULL) {  
	g <- ggplot(df) + ggtitle("chart B1") + geom_bar(aes(x=xx, y=yy, fill=fill), position="fill", stat="identity", show.legend=more$legend);  
	g <- g + scale_fill_manual(values=more$color_mapping, name='');  
	return(fmt_chart(g)); } 
chart3 <- function(df, more=NULL) {  
	g <- ggplot(df) + ggtitle("chart B1") + geom_bar(aes(x=xx, y=yy, fill=fill), position="dodge", stat="identity", show.legend=more$legend); 
	g <- g + scale_fill_manual(values=more$color_mapping, name='');  
	return(fmt_chart(g)); } 
     
chart4 <- function(df, more=NULL) {  
	g <- ggplot(df) + theme_void() + geom_bar(aes(x=xx, y=0, fill=fill), position="dodge", stat="identity", show.legend=TRUE); 
	g <- g + scale_fill_manual(values=more$color_mapping, name='');  
	return(g); } 
fmt_chart <- function(g) { 
    g <- g + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
    g <- g + theme(axis.text.x = element_text(angle = more$xx_angle, size=more$xx_size)); 
    return(g); } 
main <- function() { 
    ldf <- list(A=chart1(df=df, more=more), B=chart2(df=df, more=more), C=chart3(df=df, more=more), D=chart4(df=df, more=more) ); 
    g <- ggplot() + theme_void() + geom_place(arrangeGrob(grobs=ldf, ncol=more$array_ncol)); 
    g <- g + ggtitle(more$array_title) + labs(caption=more$array_caption); 
    print(g); } 
	 
	export_png(expr=main, path=save_as, dual=TRUE); 
} 
draw_SBS_tops <- function() { 
	df <- rename(dataset, "xx", "yy", "fill", "panel"); 
	 
	save_as <- which(df$panel[1] == list_stores); 
	save_as = make_output_file(name = sprintf("store-%02d/page3-tops.png", save_as) ); 	 
    array_title <- sprintf('Customer tops for %s with total %s', df$panel[1], fmt_c1(sum(df$yy)), save_as); 
     
    more <- list(xx_angle=25, xx_size=14, legend=TRUE,  
	    array_ncol=2, array_title=array_title, array_caption=vars$source1); 
	 
chart1 <- function(df, more=NULL) {  
	g <- ggplot(df) + ggtitle("chart A1") + geom_bar(aes(x=xx, y=yy, fill=fill), stat="identity", show.legend=more$legend); 
	g <- g + scale_fill_md5(vals=df$fill);  
	return(fmt_chart(g)); } 
chart2 <- function(df, more=NULL) {  
	g <- ggplot(df) + ggtitle("chart B1") + geom_bar(aes(x=xx, y=yy, fill=fill), position="fill", stat="identity", show.legend=more$legend);  
	g <- g + scale_fill_md5(vals=df$fill);  
	return(fmt_chart(g)); } 
chart3 <- function(df, more=NULL) {  
	g <- ggplot(df) + ggtitle("chart B1") + geom_bar(aes(x=xx, y=yy, fill=fill), position="dodge", stat="identity", show.legend=more$legend); 
	g <- g + scale_fill_md5(vals=df$fill);  
	return(fmt_chart(g)); } 
chart4 <- function(df, more=NULL) {  
	g <- ggplot(df) + theme_void(); 
	return(g); } 
fmt_chart <- function(g) { 
	g <- g + scale_fill_md5(vals=df$fill);  
    g <- g + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
    g <- g + theme(axis.text.x = element_text(angle = more$xx_angle, size=more$xx_size)); 
    return(g); } 
     
main <- function() {  
    ldf <- list(A=chart1(df, more=more), B=chart2(df, more=more), C=chart3(df, more=more), D=chart4(df, more=more) ); 
    g <- ggplot() + theme_void() + geom_place(arrangeGrob(grobs=ldf, ncol=more$array_ncol)); 
    g <- g + ggtitle(more$array_title) + labs(caption=more$array_caption); 
    print(g); }     
	export_png(expr=main, path=save_as, dual=TRUE); 
} 
draw_SBS_seasons <- function() { 
	df <- rename(dataset, "xx", "yy", "fill", "panel", "tag"); 
	 
	save_as <- which(df$panel[1] == list_stores); 
	save_as = make_output_file(name = sprintf("store-%02d/page2-seasons.png", save_as) ); 	 
    array_title <- sprintf('Customer seasons for %s with total %s', df$panel[1], fmt_c1(sum(df$yy)), save_as); 
     
    color_mapping <- scale_fill_grad(vals=df$fill, col='green', mode="map"); 
     
    more <- list(xx_angle=25, xx_size=14, legend=FALSE, color_mapping=color_mapping,  
	    array_ncol=2, array_title=array_title, array_caption=vars$source1); 
	 
sea_chart1 <- function(df, more=NULL) {  
	g <- ggplot(df) + ggtitle("chart A1") + geom_bar(aes(x=xx, y=yy, fill=fill), position="dodge", stat="identity", show.legend=more$legend); 
	g <- g + scale_fill_manual(values=more$color_mapping, name='');  
	return(fmt_chart(g, more)); } 
sea_chart2 <- function(df, more=NULL) {  
	g <- ggplot(df) + ggtitle("chart B1");  
	g <- g + geom_rect(aes(xmin=xx, xmax=xx, ymin=yy, ymax=yy, fill=fill), show.legend=more$legend) 
	 
	ldf <- split(df, df$fill); 
	for(nk in names(ldf)) { 
		ddd <- ldf[[nk]]; 
		ck <- more$color_mapping[[nk]]; 
		g <- g + geom_line(data=ddd, aes(x=xx, y=yy, group=fill), color=ck, size=1.5, show.legend=FALSE) 
		g <- g + geom_point(data=ddd, aes(x=xx, y=yy, group=fill), color=ck, size=1.7, show.legend=FALSE) 
	}	 
	 
	g <- g + scale_fill_manual(values=more$color_mapping, name='');  
	return(fmt_chart(g, more)); } 
fmt_chart <- function(g, more) { 
    g <- g + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
    g <- g + theme(axis.text.x = element_text(angle = more$xx_angle, size=more$xx_size)); 
    return(g); } 
		 
main <- function() {	 
	df$xx <- factor_JanDec(df$xx);		 
	df_mm <- df[df$tag=="mm", ]; 
	df_qq <- df[df$tag=="qq", ]; 
	 
    ldf <- list(A=sea_chart1(df_mm, more=more), B=sea_chart1(df_qq, more=more), C=sea_chart2(df_mm, more=more), D=sea_chart2(df_qq, more=more) ); 
    ldf <- arrangeGrob(grobs=ldf, layout_matrix=rbind(c(1, 1, 2), c(3, 3, 4)) );     
    g <- ggplot() + geom_place(ldf) + ggtitle(more$array_title) + labs(caption=more$array_caption); 
    print(g); } 
     
	export_png(expr=main, path=save_as, dual=TRUE); 
} 
draw_frame_cust_lead <- function(df=dataset, legend=FALSE, more=NULL) { 
	if( is.null(more) ) { 
		more <- list(legend=legend, xx_angle=15, xx_size=8); 
	} 
	 
    chart1 <- function(df, more=NULL) {  
    	g <- ggplot(df) + ggtitle("chart A1") + geom_bar(aes(x=xx, y=yy, fill=fill), stat="identity", show.legend=more$legend); 
    	return(fmt_chart(g)); } 
    	 
    chart2 <- function(df, more=NULL) {  
    	g <- ggplot(df) + ggtitle("chart B1") + geom_bar(aes(x=xx, y=yy, fill=fill), position="fill", stat="identity", show.legend=more$legend);  
    	return(fmt_chart(g)); } 
    	 
    ldf <- list(); 
    ldf$A <- chart1(df=df[df$name=="cust seg", ], more=more);  
    ldf$B <- chart1(df=df[df$name=="cust par", ], more=more); 
     
    ldf$C <- chart2(df=df[df$name=="cust seg", ], more=more);  
    ldf$D <- chart2(df=df[df$name=="cust par", ], more=more); 
    grid.arrange(grobs=ldf, ncol=2); 
} 
HP_PREV <- list(FY16='FY15', FY17='FY16', FY18='FY17', FY19='FY18', FY20='FY19', FY21='FY20', YTD22='YTD21'); 
set_frame_size_left75 <- function(scale=1.4) { 
	PANEL_WIDTH <<- floor(662 * scale); 
	PANEL_HEIGHT <<- floor(582 * scale); 
} 
set_frame_size_full <- function(scale=1.4) { 
	PANEL_WIDTH <<- floor(1000 * scale); 
	PANEL_HEIGHT <<- floor(585 * scale); 
} 
draw_BSP_bridges <- function() { 
    df <- rename(dataset, "xx", "yy", "fill", "panel"); 
	save_as = make_output_file(name = "lead/page5-BSP-bridges.png" ); 	 
    color_mapping <- scale_fill_md5(vals=df$fill, mode="map"); 
     
    more <- list(name_angle=0, date_angle=25, xx_angle=25, xx_size=11, legend=FALSE, color_mapping=color_mapping, 
	    array_ncol=4, array__title='primary bridges', array_caption=vars$source1); 
	     
draw_chart1 <- function(df, more) { 
	g <- ggplot(df) + ggtitle(sprintf("bridge for %s", df$panel[1])); 
	g <- g + geom_head(df); 
	return(fmt_chart(g, more) ); } 
main <- function() { 
    ldf <- lapply(split(df, df$panel), FUN=function(ddd) { draw_chart1(df=ddd, more=more); }); 
    g <- ggplot() + theme_void() + geom_front(arrangeGrob(grobs=ldf, ncol=1)) + ggtitle_if(more$array_title) + ggcaption_if(more$array_caption); 
    print(g); } 
	set_frame_size_full(); 
	export_png(expr=main, path=save_as, dual=TRUE); 
} 
draw_BSP_root_view <- function() { 
    df <- rename(dataset, "xx", "yy", "fill", "panel"); 
     
	save_as = make_output_file(name = "lead/page4-primary-cuts-GRCT-view.png" ); 	 
    color_mapping <- scale_fill_md5(vals=df$fill, mode="map"); 
     
    more <- list(name_angle=0, date_angle=25, xx_angle=25, xx_size=11, legend=FALSE, color_mapping=color_mapping, 
	    array_ncol=4, array_x_title="primary cuts via GRCT triplet", array_caption=vars$source1); 
     
draw_carousel1 <- function(df, more) { 
	tdf <- make_GRCT_triplet(df=df, mode="df+");  
    g <- ggplot() + theme_void() + geom_head(tdf) + ggtitle(sprintf("GRCT triplet for %s", df$panel[1])); 
	return(g); }	 
	 
main <- function() { 
    ldf <- lapply(split(df, df$panel), FUN=function(ddd) { draw_carousel1(df=ddd, more=more); }); 
    g <- ggplot_void(arrangeGrob(grobs=ldf, ncol=1)) + ggtitle_if(more$array_title) + ggcaption_if(more$array_caption); 
    print(g); } 
	set_frame_size_full(); 
	export_png(expr=main, path=save_as, dual=TRUE); 
} 
draw_BSP_gantt_chart <- function(box_height=0.45) { 
    df <- rename(dataset, "code", "yy", "DMI", "DMO"); 
    df$fill <- df$code; 
	codes <- rev(unique_vals(df$code));     
	df$code <- factor(df$code, levels=codes); 
    df$y1 <- as.integer(df$code) - box_height; 
    df$y2 <- df$y1 + 2 * box_height; 
     
    dates <- unique_vals(df$DMI, df$DMO); 
    df$DMI <- factor(df$DMI, levels=dates); df$x1 <- as.integer(df$DMI); 
    df$DMO <- factor(df$DMO, levels=dates); df$x2 <- as.integer(df$DMO); 
	save_as = make_output_file(name = "lead/page3-gantt.png" ); 	 
    color_mapping <- scale_fill_md5(vals=df$fill, mode="map"); 
     
    more <- list(name_angle=0, date_angle=25, xx_angle=25, xx_size=11, legend=FALSE, color_mapping=color_mapping, 
	    array_ncol=4, array_title='xx', array_caption=vars$source1); 
main <- function() { 
    g <- ggplot(df); 
    g <- g + geom_text(aes(x=DMI, y=code, label=''), show.legend=FALSE); 
    g <- g + geom_text(aes(x=DMO, y=code, label=''), show.legend=FALSE); 
    g <- g + geom_rect(aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=fill), show.legend=FALSE); 
    g <- g + geom_text(aes(x=DMI, y=code, label=DMO, fill=fill), angle=more$date_angle, show.legend=FALSE); 
    g <- g + geom_text(aes(x=DMO, y=code, label=DMO, fill=fill), angle=more$date_angle, show.legend=FALSE); 
    g <- g + geom_text(aes(x=(x1+x2)/2, y=(y1+y2)/2, label=code), angle=more$name_angle, show.legend=FALSE); 
    g <- g + scale_fill_manual(values=more$color_mapping, name=''); 
    print(fmt_chart(g, more=more) ); } 
     
	set_frame_size_left75(); 
	export_png(expr=main, path=save_as, dual=TRUE); 
} 
draw_BSP_subsets_in_aspects <- function() { 
	df <- rename(dataset, "xx", "yy", "fill", "panel"); 
	 
	save_as = make_output_file(name = "lead/page2-subsets-in-aspects.png" ); 	 
    array_title <- sprintf('subsets in aspects with total %s', fmt_c1(sum(df$yy)), save_as); 
     
    color_mapping <- scale_fill_md5(vals=df$fill, mode="map"); 
     
    more <- list(xx_angle=65, xx_size=11, legend=FALSE, color_mapping=color_mapping, 
	    array_ncol=4, array_title=array_title, array_caption=vars$source1); 
draw_chart1 <- function(df, more) { 
	g <- ggplot(df) + geom_bar_xyfill(legend=TRUE) + ggtitle(sprintf("SBC for %s", df$panel[1])); 
	g <- g + scale_fill_md5(vals=df$fill, mode=""); 
	return(fmt_chart(g, more=more)); } 
	 
draw_chart2 <- function(df, more) { 
	g <- ggplot(df) + geom_bar_xyfill(pos="fill", legend=FALSE) + ggtitle(sprintf("FBC for %s", df$panel[1])); 
	g <- g + scale_fill_md5(vals=df$fill, mode=""); 
	return(fmt_chart(g, more=more)); } 
	 
draw_chart3 <- function(df, more) { 
	g <- ggplot(df) + geom_bar_xyfill(pos="dodge", legend=FALSE) + ggtitle(sprintf("SBC for %s", df$panel[1])); 
	g <- g + scale_fill_md5(vals=df$fill, mode=""); 
	return(fmt_chart(g, more=more)); } 
	 
draw_carousel1 <- function(df, more) { 
	ldf <- list(A=draw_chart1(df=df, more=more), B=draw_chart2(df=df, more=more), C=draw_chart3(df=df, more=more)); 
    lmt <- rbind(c(1, 1, 1, 2, 2, 3, 3, 3)); 
    g <- ggplot_void(arrangeGrob( grobs=ldf, layout_matrix=lmt )); 
	return(g); }	 
	 
main <- function() { 
    ldf <- lapply(split(df, df$panel), FUN=function(ddd) { draw_carousel1(df=ddd, more=more); }); 
    lmt <- rbind(c(1, 1, 1, 2, 2, 3, 3, 3)); 
    g <- ggplot_void(arrangeGrob(grobs=ldf, ncol=1)) + ggtitle(more$array_title) + labs(caption=more$array_caption); 
    print(g); } 
	set_frame_size_left75(); 
	export_png(expr=main, path=save_as, dual=TRUE); 
} 
fmt_chart <- function(g, more=list(xx_angle=25, xx_size=11)) { 
    g <- g + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
    g <- g + theme(axis.text.x = element_text(angle = more$xx_angle, size=more$xx_size)); 
    return(g); } 
draw_BSP_store_listing_full <- function() { 
	draw_BSP_store_listing(full_frame=TRUE); 
} 
draw_BSP_store_listing <- function(full_frame=FALSE) { 
	df <- rename(dataset, "xx", "yy", "fill", "panel"); 
	save_as = make_output_file(name = sprintf("lead/page1-store-array%s.png", ifelse(full_frame, "-full", "") ) ); 	 
    array_title <- sprintf('trends for stores with total %s', fmt_c1(sum(df$yy)), save_as); 
     
    color_mapping <- scale_fill_md5(vals=df$fill, mode="map"); 
     
    more <- list(xx_angle=65, xx_size=11, legend=FALSE, color_mapping=color_mapping, 
	    array_ncol=4, array_title='store listing', array_caption=vars$source1); 
chart1 <- function(df, more=NULL) { 
    g <- ggplot(df) + geom_bar(aes(x=xx, y=yy, fill=fill), stat="identity", show.legend=FALSE); 
	g <- g + scale_fill_manual(values=more$color_mapping, name='');  
    return(fmt_chart(g, more)); } 
main <- function() { 
    ldf <- lapply(split(df, df$panel), FUN=function(ddd) { chart1(df=ddd, more=more) + ggtitle(ddd$panel[1]); }); 
    g <- ggplot_void(arrangeGrob(grobs=ldf, ncol=more$array_ncol)) + ggtitle(more$array_title) + labs(caption=more$array_caption); 
    print(g); } 
	if(full_frame) { set_frame_size_full(); } else { set_frame_size_left75(); } 
	export_png(expr=main, path=save_as, dual=TRUE); 
} 
make_pivot_xyfill <- function(df) { 
    df$xx <- as.character(df$xx); 
    df$fill <- as.character(df$fill); 
    rdf <- data.frame(); 
    for(k in 1:nrow(df)) { 
        rk <- df$fill[k]; 
        ck <- df$xx[k]; 
        rdf[rk, ck] <- df$yy[k]; 
    } 
    return(rdf); 
} 
make_GRCT_triplet <- function(df=rename(dataset, "xx", "yy", "fill"),  
avoid=c("FY17", "YTD21"), fmt_per=fmt_100, fmt_val=fmt_c1, cols=NULL, mode="") { 
    rdf <- make_pivot_xyfill(df); 
    rows <- row.names(rdf); 
    if( is.null(cols) ) cols <- names(rdf); 
    rdf <- rdf[, cols];  
    rdf_GR <- data.frame(); 
    rdf_CT <- data.frame(); 
    tdf <- lapply(split(df, df$xx), FUN=function(ddd) { sum(ddd$yy); }); 
    for(k in 1:nrow(rdf)) for(j in seq_along(cols)) {  
        ty <- cols[j];  ty_val <- rdf[k, ty]; 
        rdf_GR[k, ty] <- fmt_per(if( ty %in% avoid) { NA } else { ly <- cols[j-1]; ly_val <- rdf[k, ly]; (ty_val - ly_val)/ly_val; }); 
        rdf_CT[k, ty] <- fmt_per(ty_val / tdf[[ty]]); 
    } 
     
    for(k in 1:nrow(rdf)) for(cj in cols) { rdf[k, cj] <- fmt_val(as.double(rdf[k, cj])); }  
    df <- data.frame(name=rows); row.names(rdf) <- NULL; 
     
    if(mode == "df+" ) { 
    	rdf <- cbind(df, rdf, rdf_GR, rdf_CT); 
    	return(rdf); 
    } 
     
    rdf <- cbind(df, rdf); 
    rdf_GR <- cbind(df, rdf_GR); 
    rdf_CT <- cbind(df, rdf_CT); 
    if(mode == "df" ) { 
        ldf <- list( amt=rdf, GR=rdf_GR, CT=rdf_CT, cols=cols, rows=rows, totals=tdf, avoid=avoid ); 
        return(ldf); 
    } 
    
    ldf <- list(AAA=tableGrob(rdf), BBB=tableGrob(rdf_GR), CCC=tableGrob(rdf_CT)); 
    g <- ggplot() + theme_void() + geom_array(grobs=ldf, ncol=1); 
    print(g); 
} 
draw_BBC_bridge <- function(df = rename(dataset, "xx", "yy", "fill"),  
more=NULL, edit_more=NULL) { 
	more <- list(xx_angle=25, xx_size=11, box_width=0.45, minor_sep = "/", 
		minor_back = list('FY18'='FY17', 'FY19'='FY18', 'FY20'='FY19', 'FY21'='FY20', 'YTD22'='YTD21'),  
		major_angle=0, minor_angle=25, major_place=0.5,   
		fmt_major=fmt_c1, fmt_minor=fmt_c1, debug=FALSE, debug_top=17, export=save_as_HTML_table, export_as=NULL, legend=TRUE); 
	if( !is.null(edit_more) ) more <- edit_more(more); 
    df_major <- build_major_part_xyfill(df, more=more); 
    df_minor <- build_minor_part_xyfill(df, more=more); 
    df <- df_major; 
     
 #   g <- draw_BBC_trails(df, more); 
 	g <- ggplot(df) + geom_head(df); 
 	 
    print(g); 
} 
build_major_part_xyfill <- function(df, more=NULL) { 
    tdf <- lapply(split(df, df$xx), FUN=function(ddd) { sum(ddd$yy); }); 
    rdf <- data.frame(); 
     
    for(nk in names(tdf)) { rdf[nk, "xx"] <- nk; rdf[nk, "yy"] <- tdf[[nk]]; rdf[nk, "fill"] <- "major"; 
        rdf[nk, "yy1"] <- NA; rdf[nk, "diff"] <- NA; 
        rdf[nk, "xx_full"] <- nk; rdf[nk, "xx_half"] <- nk; rdf[nk, "xx_split"] <- "major"; rdf[nk, "yy_tall"] <- tdf[[nk]];         
    } 
    return(rdf); 
} 
build_minor_part_xyfill <- function(df, more=NULL) { 
	sep <- more$minor_sep; 
	back <- more$minor_back; 
	 
    row.names(df) <- paste(df$xx, df$fill, sep=sep); 
    for(k in 1:nrow(df)) { 
        ck <- as.character(df$fill[k]); ty <- as.character(df$xx[k]); ly <- back[[ty]]; 
        if( is.null(ly) ) {  } else { k_1 = paste(ly, ck, sep=sep); df[k_1, "yy1"] <- df[k, "yy"]; df[k_1, "diff"] <- df[k, "yy"] - df[k_1, "yy"];   }   
    } 
    df$xx_full <- row.names(df); 
    df$xx_half <- df$fill; 
    df$xx_split <- 'minor'; 
    df$yy_tall <- df$diff; 
    return(df); 
} 
merge_MM_parts <- function(df_major, df_minor, more) { 
    df <- rbind(df_major, df_minor);     
    df <- df[order(df$xx_full), ]; 
    df <- remove_minor_parts(df);     
     
    df$x1 <- as.integer(factor(df$xx_full)) - more$box_width; 
    df$x2 <- df$x1 + 2*more$box_width; 
     
    s <- 0; 
    for(k in 1:nrow(df)) { 
    	if(df$xx_split[k] == "major" ) { s <- df$yy_tall[k]; } else { df[k, "y1"] <- s; s <- s + df[k, "yy_tall"]; df[k, "y2"] <- s;  } 
    } 
     
    return(df); 
} 
remove_minor_parts <- function(df) { 
	sel <- ( df$xx_split=="minor" & (df$xx=="FY21" | df$xx=="YTD22") ); 
	df <- df[!sel, ];	 
	return(df); 
}	 
draw_BBC_trails <- function(df, more) { 
    g <- ggplot(df); 
    g <- g + geom_text(aes(x=xx_full, y=yy_tall, label=''), show.legend=FALSE); 
     
    df_minor <- df[df$xx_split=="major", ]; 
    g <- g + geom_bar(data=df_major, aes(x=xx_full, y=yy_tall, fill=fill), stat="identity", show.legend=more$legend); 
    g <- g + geom_text(data=df_major, aes(x=xx_full, y=yy_tall*more$major_place, label=more$fmt_major(yy_tall)),  
    	angle=more$major_angle, show.legend=FALSE); 
     
    df_minor <- df[df$xx_split=="minor", ]; 
    g <- g + geom_rect(data=df_minor, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=fill), show.legend=more$legend); 
    g <- g + geom_text(data=df_minor, aes(x=(x1+x2)/2, y=(y1+y2)/2, label=more$fmt_minor(yy_tall)), angle=more$minor_angle, show.legend=FALSE); 
     
    g <- g + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
    g <- g + theme(axis.text.x = element_text(angle = more$xx_angle, size=more$xx_size)); 
    g <- g + scale_x_discrete(labels=df$xx_half); 
     
    if(more$debug) g <- g + geom_head(df, top=more$debug_top); 
	 
	if( !is.null(more$export) ) more$export(df, path=more$export_as);  
	return(g); 
} 
save_as_HTML_table <- function(df, path=NULL, single=TRUE) { 
	if(is.null(path) ) path <- make_output_file(name='out1.html'); 
	if(single) sink(path); 
	 
	cat("<table border=1>\n"); 
	 
	if(TRUE) { 
		cols <- names(df); 
		cat("<tr>"); 
		for(cj in cols) { cat("<th>", cj,"</th>"); }		 
		cat("</tr>"); 
	} 
	 
	 
	for(k in 1:nrow(df) ) { 
		cat("<tr>"); 
		for(j in 1:ncol(df)) { sjk <- df[k, j]; cat("<td>", as.character(sjk) ,"</td>"); } 
		cat("</tr>"); 
	} 
	 
	cat("</table>\n"); 
	 
	if(single) for(k in seq_len(sink.number()) ){ sink(NULL); } 
}   
ggtitle_AUD <- function(fmt, df) { 
    if( is.null(fmt) ) return(fmt); 
    pk <- df$panel[1]; 
    yk <- fmt_c1(sum(df$yy)); 
    ggtitle(sprintf(fmt, pk, yk)); 
} 
make_color_md5_AUD <- function(nk, seed='abc', sk=1, ek=6) { 
    nk <- md5(paste(nk, seed)); 
    nk <- paste0('#', substr(nk, sk, ek));   
    return(nk); 
} 
draw_line_chart_xyfill_AUD <- function(df=NULL, more=NULL) { 
    g <- ggplot(df); 
    ldf <- split(df, df$fill); color_mapping <- list(); 
    for(nk in names(ldf) ) { 
        ddd <- ldf[[nk]]; ck <- more$make_color(nk); color_mapping[[nk]] <- ck; 
        g <- g + geom_line(data=ddd, aes(x=xx, y=yy, group=fill), color=ck, show.legend=FALSE); 
        g <- g + geom_point(data=ddd, aes(x=xx, y=yy, group=fill), color=ck, show.legend=FALSE); 
        g <- g + geom_rect(data=ddd, aes(xmin=xx, ymin=yy, xmax=xx, ymax=yy, fill=fill), show.legend=more$legend); 
    } 
    g <- g + scale_fill_manual(values=color_mapping, name=''); 
    return(more$fmt_chart(g, more=more)); 
} 
draw_bar_chart_xyfill_AUD <- function(df=NULL, more=NULL) { 
    g <- ggplot(df);     
    g <- g + geom_bar_xyfill(pos="stack", legend=more$legend);     
    return(more$fmt_chart(g, more=more)); 
} 
draw_full_bar_chart_xyfill_AUD <- function(df=NULL, more=NULL) { 
    g <- ggplot(df);     
    g <- g + geom_bar_xyfill(pos="fill", legend=more$legend);     
    return(more$fmt_chart(g, more=more)); 
} 
draw_d2d_duet_AUD <- function(df=rename(dataset, "xx", "yy", "fill", "panel"), more=NULL, edit_more=NULL) { 
    more <- list(make_left=draw_bar_chart_xyfill_AUD, make_right=draw_full_bar_chart_xyfill_AUD, 
    	make_color=make_color_md5_AUD, fmt_chart=fmt_chart, 
        inner_ncol=2, array_ncol=2, array_caption='source: xx', chart_title='Bar chart for [%s] with total [%s]', legend=FALSE); 
         
	more$save_as = make_output_file(name = "cust/page2-TB-duet.png" ); 	  
	 
main <- function() { 
    if( !is.null(edit_more) ) more <- edit_more(more); 
	ldf <- split(df, df$panel); 
	 
    ldf_LL <- lapply(ldf, FUN=function(ddd) { more$make_left(df=ddd, more=more) + ggtitle_AUD(fmt=more$chart_title, df=ddd); }); 
    ldf_RR <- lapply(ldf, FUN=function(ddd) { more$make_right(df=ddd, more=more) + ggtitle_AUD(fmt=more$chart_title, df=ddd); }); 
     
    ldf_LL <- arrangeGrob(grobs=ldf_LL, ncol=more$inner_ncol); 
    ldf_RR <- arrangeGrob(grobs=ldf_RR, ncol=more$inner_ncol); 
    g <- ggplot() + theme_void() + geom_array(grobs=list(LL=ldf_LL, RR=ldf_RR), ncol=more$array_ncol) + ggcaption_if(more$aray_caption); 
    print(g); 
} 
	set_frame_size_left75(); 
	export_png(expr=main, path=more$save_as, dual=TRUE);  
} 
draw_page_cust_line_array <- function(df=rename(dataset, "xx", "yy", "fill", "panel"), more=NULL, edit_more=NULL) { 
    more <- list(make_chart=draw_line_chart_xyfill_AUD, make_color=make_color_md5_AUD, fmt_chart=fmt_chart, 
        array_ncol=2, array_caption='source: xx', chart_title='Line chart for [%s] with total [%s]', legend=TRUE); 
         
	more$save_as = make_output_file(name = "cust/page1-line-array.png" ); 	  
main <- function() { 
    if( !is.null(edit_more) ) more <- edit_more(more); 
    ldf <- lapply(split(df, df$panel), FUN=function(ddd) { more$make_chart(df=ddd, more=more) + ggtitle_AUD(fmt=more$chart_title, df=ddd); }); 
    g <- ggplot() + theme_void() + geom_array(grobs=ldf, ncol=more$array_ncol) + ggcaption_if(more$aray_caption); 
    print(g); 
} 
	set_frame_size_left75(); 
	export_png(expr=main, path=more$save_as, dual=TRUE);  
} 
factor_uniq <- function(vals, dec=TRUE) { 
    voc <- unique_values(vals=vals, dec=dec); 
    vals <- factor(vals, levels=voc); 
	return(vals) 
} 
fmt_triadic_value <- function(val, type) { 
	if(type=="amt") return( fmt_c1(val) );  
	return( fmt_100(val) ); 
} 
lappy_frame <- function(ldf, name="name", val="val", FUN) { 
    ldf <- lapply(ldf, FUN=FUN); 
    df <- data.frame(); 
    for(nk in names(ldf)) { df[nk, name] <- nk; df[nk, val] <- ldf[[nk]] } 
    return(df); 
} 
draw_waterfall_array <- function(df=rename(dataset, "xx", "yy", "fill", "panel", "type"), more=NULL, edit_more=NULL) { 
	more <- list(xx_angle=25, xx_size=11, array_ncol=2); 
    if( !is.null(edit_more) ) more <- edit_more(more);  
chart1 <- function(df, more) { 
	g <- ggplot(df); 
	g <- g + geom_bar(aes(x=xx, y=yy, fill=fill), stat="identity", show.legend=TRUE); 
	 
    g <- g + theme(axis.text.x = element_text(angle = more$xx_angle, size=more$xx_size)); 
	return(g); 
} 
	ldf <- lapply(split(df, df$panel), FUN=function(ddd) { chart1(df=ddd, more=more); }); 
	g <- ggplot() + theme_void() + geom_array(grobs=ldf, ncol=more$array_ncol); 
	print(g); 
} 
draw_triadic_view <- function(df=rename(dataset, "xx", "yy", "fill", "panel", "metric"), more=NULL) { 
	more <- list(xx_angle=90, xx_size=11, fill_sep=" >> ", fiscal_sep=" ", row_break=TRUE, col_break=TRUE); 
    df$xx_metric <- paste(df$metric, df$xx, sep=more$fiscal_sep); 
    df$yy_fill_panel <- factor_uniq(paste(df$panel, df$fill, sep=more$fill_sep)); 
    for(k in 1:nrow(df)) { df$label[k] <- fmt_triadic_value(val=df$yy[k], type=df$metric[k]); } 
     
    g <- ggplot(df); 
    g <- g + geom_text(aes(x=xx_metric, y=yy_fill_panel, label=label), hjust=1, angle=0, show.legend=FALSE); 
if(more$row_break) {         
    tdf <- lappy_frame(split(df, df$panel), FUN=function(ddd) {  
    	f1 <- max(as.character(ddd$fill)); p1 <- as.character(ddd$panel[1]); paste(p1, f1, sep=more$fill_sep) }); 
    	 
    g <- g + geom_text(data=tdf, aes(x=1, y=name, label='a'), hjust=1, angle=0, show.legend=FALSE); 
} 
if(more$col_break) {         
    tdf <- lappy_frame(split(df, df$metric), FUN=function(ddd) {  
    	f1 <- max(as.character(ddd$xx)); p1 <- as.character(ddd$metric[1]); paste(p1, f1, sep=more$fiscal_sep) }); 
    	 
    g <- g + geom_text(data=tdf, aes(y=1, x=name, label='b'), hjust=1, angle=0, show.legend=FALSE); 
} 
     
    g <- g + scale_x_discrete(position = "top");  
    g <- g + theme(axis.text.x = element_text(angle = more$xx_angle, size=more$xx_size)); 
     
    g <- g + theme(axis.title.x = element_blank(), axis.title.y = element_blank()); 
	print(g);     
}  
draw_pivotal_quintet <- function(df=rename(dataset, "xx", "yy", "fill", "panel"),  
more=NULL, edit_more=NULL) { 
chart1 <- function(df=NULL, more=NULL) { ggplot() + ggtitle("A"); } 
chart2 <- function(df=NULL, more=NULL) { ggplot() + ggtitle("B"); } 
chart3 <- function(df=NULL, more=NULL) { ggplot() + ggtitle("C"); } 
chart4 <- function(df=NULL, more=NULL) { ggplot() + ggtitle("D"); } 
chart5 <- function(df=NULL, more=NULL) { ggplot() + ggtitle("E"); } 
main <- function() { 
    ldf <- list(A=chart1(df=df, more=more), B=chart2(df=df, more=more), C=chart3(df=df, more=more), 
                D=chart4(df=df, more=more), E=chart5(df=df, more=more)); 
    ncol <- rbind(c(1, 2), c(3, 4), c(5, 5)); 
    g <- ggplot(df) + geom_front(arrangeGrob(grobs=ldf, layout_matrix=ncol)); 
    print(g); 
} 
main(); 
} 
make123_major_bars_123 <- function(df, more=NULL) { 
    rdf <- data.frame(); 
    df$xx_dual <- paste(df$xx_major, df$xx_minor, sep=more$fiscal_sep); 
    tdf_major <- lapply(split(df, df$xx_major), FUN=function(ddd) { sum(ddd$yy); }); 
    tdf_minor <- lapply(split(df, df$xx_minor), FUN=function(ddd) { 0 }); 
    for(nk in names(tdf_major)) { 
        rdf[nk, "xx"] <- nk; rdf[nk, "yy"] <- tdf_major[[nk]]; rdf[nk, "fill"] <- more$major_tag;  
        	rdf[nk, "type"] <- "major"; rdf[nk, "xx_next"] <- ""; 
         
        for(nj in names(tdf_minor)) { 
            pj <- paste(nk, nj, sep=more$fiscal_sep); 
            rdf[pj, "xx"] <- pj; rdf[pj, "yy"] <- NA; rdf[pj, "fill"] <- nj; rdf[pj, "type"] <- "minor";             
            rdf[pj, "xx_next"] <- paste(next_year(nk), nj, sep=more$fiscal_sep); 
        } 
    } 
     
    tdf_dual <- lapply(split(df, df$xx_dual), FUN=function(ddd) { sum(ddd$yy); }); 
     
    for(k in 1:nrow(rdf) ) { 
    	nk <- rdf$xx[k];  
    	tryCatch({ rdf[k, "yy1"] <- tdf_dual[[nk]]; }, error=function(e) {}, warning=function(e){}); 
    	nk <- rdf$xx_next[k];  
    	tryCatch({ rdf[k, "yy2"] <- tdf_dual[[nk]]; }, error=function(e) {}, warning=function(e){}); 
    } 
     
    df <- rdf; rdf <- NULL; 
     
    sel <- sapply(df$xx_next, FUN=function(xx) { is.na(xx) | substr(xx, 1, 3) != "DEL"; }); 
    df <- df[sel, ];      
     
    df$x1 <- as.integer(factor(df$xx)) - more$box_width; 
	df$x2 <- df$x1 + 2 * more$box_width; 
	 
	df$xx_diff <- as.double(df$yy2 - df$yy1); 
	 
	s <- 0; 
	for(k in 1:nrow(df) ) { 
		if( df$type[k] == "major" ) { s <- df$yy[k]; } else { df[k, "y1"] <- s; s <- s + df$xx_diff[k]; df[k, "y2"] <- s;  } 
	} 
	 
    return(df); 
} 
next123_year_123 <- function(nk) { 
	map <- list(FY17='FY18', FY18='FY19', FY19='FY20', FY20='FY21', FY21='DEL1', YTD21='YTD22', YTD22='DEL2'); 
	return(map[[nk]]); 
} 
draw123_BBC_bridge_123 <- function(df=rename(dataset, "xx_major", "xx_minor", "yy"), more=NULL) { 
	more <- list(xx_angle=75, fmt_major=fmt_c1, fmt_minor=fmt_c1, major_place=0.5, xx_size=11,  
		box_width=0.45, major_tag="major", fiscal_sep=" ", row_break=TRUE, col_break=TRUE); 
    df <- make_major_bars(df=df, more=more); 
     
    g <- ggplot(df); 
     
    g <- g + geom_bar(aes(x=xx, y=yy, fill=fill), stat="identity", show.legend=TRUE); 
    g <- g + geom_text(aes(x=xx, y=yy*more$major_place, label=more$fmt_major(yy), group=fill), show.legend=FALSE); 
     
    df_minor <- df[df$type == "minor", ]; 
    g <- g + geom_rect(data=df_minor, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=fill), show.legend=TRUE); 
    g <- g + geom_text(data=df_minor, aes(x=(x1+x2)/2, y=(y1+y2)/2, label=fmt_c1(xx_diff), group=fill), show.legend=FALSE); 
     
    g <- g + theme(axis.text.x = element_text(angle = more$xx_angle, size=more$xx_size)); 
     
    print(g); 
} 
md5_rand <- function(s) { 
    s <- as.character(s); 
	s <- gsub("\\D+", "", md5(s)); 
	s <- as.double(paste0("0.", s)); 
	return(s); 
} 
blend_colors <- function(c1=col2rgb('red'), c2=col2rgb('green'), t=0.5, h=FALSE) { 
    if(h) t <- md5_rand(t); 
    m <- c1*t + c2*(1-t);  
    return(rgb(red=m[1], green=m[2], blue=m[3], max=255)); 
} 
blend_colors_DIR <- function(c1=col2rgb('red'), c2=col2rgb('magenta'), c3=col2rgb('cyan'), t='abcd', h=NULL) { 
	t1 <- md5_rand(paste(t, "red")); 
	t2 <- md5_rand(paste(t, "green")); 
	t3 <- md5_rand(paste(t, "blue")); 
    m <- (c1*t1 + c2*t2 + c3*t3) / (t1 + t2 + t3);  
    return(rgb(red=m[1], green=m[2], blue=m[3], max=255)); 
} 
draw_color_boxes <- function(vals=runif(10), ncol=1, make_color=NULL) { 
    ldf <- lapply(vals, FUN=function(ttt) {  
        g <- ggplot() + theme_void() + ggtitle(ttt) + geom_rect(aes(xmin=0, xmax=1, ymin=0, ymax=1), fill=make_color(t=ttt)); 
        return(g); }); 
    grid.arrange(grobs=ldf, ncol=ncol); 
}  
draw_columnar_cuts <- function(df=dataset, more=NULL, edit_more=NULL) { 
chart1 <- function(df, more) { 
    g <- ggplot(df) + geom_bar(aes(x=xx, y=yy, fill=fill), stat="identity", show.legend=TRUE); 
    g <- g + scale_fill_md5(vals=df$fill, mode=""); 
    return(g); 
} 
    rdf <- df[, 1:2]; 
    names(rdf) <- c("xx", "yy"); 
     
    ldf <- list(); cols <- names(df); cols <- cols[3:length(cols)]; 
    for(nk in cols) { 
        rdf$fill <- df[[nk]]; 
        ldf[[nk]] <- chart1(df=rdf, more=more) + ggtitle(nk); 
    } 
    g <- ggplot() + theme_void() + geom_array(grobs=ldf, ncol=2); 
    print(g); 
} 
draw123_dyadic_trend_view_123 <- function(df=rename(dataset, "xx", "yy", "fill", "panel")) { 
chart1 <- function(df, more=NULL) {     
    g <- ggplot(df); 
    g <- g + geom_bar(aes(x=xx, y=yy, fill=fill), stat="identity", show.legend=TRUE); 
    return(g); } 
chart2 <- function(df, more=NULL) {     
    g <- ggplot(df); 
    g <- g + geom_bar(aes(x=xx, y=yy, fill=fill), stat="identity", position="fill", show.legend=TRUE); 
    g <- g + geom_text(aes(x=xx, y=yy, label=fmt_c1(yy), group=fill), position=position_fill(vjust=0.5), show.legend=FALSE); 
    return(g); } 
    ldf <- list(A=chart1(df=df), B=chart2(df=df)); 
    g <- ggplot() + theme_void() + geom_array(grobs=ldf, ncol=2) + ggtitle('dyadic view of operational revenue'); 
    print(g); 
} 
draw_triadic_trend_view_123 <- function(df=rename(dataset, "xx", "yy", "fill", "panel"), 
more=NULL, edit_more=NULL) { 
chart1 <- function(df, more=NULL) {     
    g <- ggplot(df); 
    g <- g + geom_bar(aes(x=xx, y=yy, fill=fill), stat="identity", show.legend=more$legend); 
    g <- g + geom_text(aes(x=xx, y=yy, label=more$fmt_text(yy), group=fill), position=position_stack(vjust=0.5), show.legend=FALSE); 
    return(more$fmt_chart(g)); } 
chart3 <- function(df, more=NULL) {     
    g <- ggplot(df); 
    g <- g + geom_bar(aes(x=xx, y=1, fill=fill), stat="identity", position="fill", show.legend=FALSE); 
    g <- g + geom_text(aes(x=xx, y=1, label=more$fmt_100(yy_CT), group=fill), position=position_fill(vjust=0.5), show.legend=FALSE); 
    return(more$fmt_chart(g)); } 
chart2 <- function(df, more=NULL) {     
	for(k in 1:nrow(df)) { df$yy_GRR[k] <- more$fmt_100(df$yy_GR[k]); } 
	 
    g <- ggplot(df); 
    g <- g + geom_bar(aes(x=xx, y=1, fill=fill), stat="identity", position="fill", show.legend=FALSE); 
    g <- g + geom_text(aes(x=xx, y=1, label=yy_GRR, group=fill), position=position_fill(vjust=0.5), show.legend=FALSE); 
    return(more$fmt_chart(g)); } 
chart4 <- function(df, more=NULL) {     
    g <- ggplot(df) + theme_void(); 
    g <- g + geom_bar(aes(x=fill, y=1, fill=fill), stat="identity", show.legend=FALSE); 
    g <- g + geom_text(aes(x=fill, y=1, label=fill, group=fill), hjust="inward", show.legend=FALSE); 
    g <- g + theme(axis.title.x = element_blank(), axis.title.y = element_blank()); 
    g <- g + theme(axis.text.x = element_blank(), axis.text.y = element_blank()); 
    g <- g + coord_flip(); 
    return(g); } 
     
main <- function() { 
	more <- list(fmt_chart=fmt_chart, fmt_text=fmt_c1, fmt_100=fmt_100, legend=FALSE, 
	   array_ncol=3, array_title='triadic view of operational revenue', xx_sep="/"); 
	    
	df <- add_GRCT_labels(df, more);    
		 
    ldf <- list(A=chart1(df=df, more=more), B=chart2(df=df, more=more), C=chart3(df=df, more=more), C=chart4(df, more=more)); 
    ncol <- rbind(c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4)); 
    g <- ggplot() + theme_void() + geom_front(arrangeGrob(grobs=ldf, layout_matrix=ncol)) + ggtitle(more$array_title); 
    print(g); } 
     
    main(); 
} 
last_year_123 <- function(xk) { 
	map <- list(FY17='NA16', FY18='FY17', FY19='FY18', FY20='FY19', FY21='FY20', LTM22='FY21', YTD21='NA20', YTD22='YTD21'); 
	return(map[[xk]]); }     
add_GRCT_labels_123 <- function(df, more) { 
	row.names(df) <- paste(df$xx, df$fill, sep=more$xx_sep);    
	tdf <- lapply(split(df, df$xx), FUN=function(ddd) { sum(ddd$yy) }); 
	 
	 
	for(k in 1:nrow(df)) { 
		xk <- as.character(df$xx[k]); 
		fk <- as.character(df$fill[k]); 
		 
		tk <- tdf[[xk]];  
		df$yy_foot[k] <- tk; df$yy_CT[k] <- df$yy[k] / tk; 
		 
		lk <- paste(last_year(xk), fk, sep=more$xx_sep); yy_last <- df[lk, "yy"]; 
		df$xx_last[k] <- lk; df$yy_last[k] <- yy_last;  
		df$yy_GR[k] <- if( is.na(yy_last) ) { NA } else { (df$yy[k] - df$yy_last[k])/df$yy_last[k]; } 
	} 
	 
	return(df); 
} 
draw_categorical_bridge_NOVEL <- function(df=rename(dataset, "xx", "yy", "fill", "panel", "fill_new"),  
more=NULL, edit_more=NULL) { 
	more <- list(last_year=last_year_FY17_YTD22, next_year=next_year_FY17_YTD22, xx_angle=15, fmt_major=fmt_c1, fmt_minor=fmt_c1, major_place=0.5, xx_size=11,  
		debug_table = TRUE, debug_top = 25, 
		box_width=0.45, major_tag="major", fiscal_sep=" ", row_break=TRUE, col_break=TRUE); 
	if( !is.null(edit_more) ) more <- edit_more(more);  
	df <- compute_bridge_totals(df, more); 
     
    g <- ggplot(df); 
    g <- g + geom_text(aes(x=xx, y=0, label='', group=fill), show.legend=FALSE); 
    df_major <- df; 
    g <- g + geom_bar(data=df_major, aes(x=xx, y=yy, fill=fill), stat="identity", show.legend=TRUE); 
    g <- g + geom_text(data=df_major, aes(x=xx, y=yy*more$major_place, label=more$fmt_major(yy), group=fill), show.legend=FALSE); 
     
    df_minor <- df[df$type == "minor", ]; 
    g <- g + theme(axis.text.x = element_text(angle = more$xx_angle, size=more$xx_size)); 
	g <- g + scale_x_discrete(labels=df$xx_short); 
	     
    if(more$debug_table) g <- g + geom_head(df, top=more$debug_top);     
    print(g); 
} 
lapply_split_sum <- function(df, spl) { 
	 lapply(split(df, spl), FUN=function(ddd) { sum(ddd$yy) }); 
} 
lapply_split0_sum <- function(df, name) { 
	 spl <- df[[name]]; 
	 lapply(split(df, spl), FUN=function(ddd) { sum(ddd$yy) }); 
} 
lapply_split_set <- function(df, spl) { 
	 lapply(split(df, spl), FUN=function(ddd) { 0 }); 
} 
compute_bridge_totals <- function(df, more=NULL) { 
	df$xx_last <- sapply(df$xx, FUN=function(xx) { more$last_year(xx) }); 
	df$xx_fill <- paste(df$xx, df$fill, sep=more$fiscal_sep); 
	df$xx_fill_last <- paste(df$xx_last, df$fill, sep=more$fiscal_sep); 
	 
	rdf <- data.frame(); 
	 
	tdf_major <- lapply_split_sum(df, df$xx); 
	tdf_minor <- lapply_split_set(df, df$fill); 
		 
	for(nk in names(tdf_major)) { 
		rdf[nk, "xx"] <- nk; rdf[nk, "xx_short"] <- nk; rdf[nk, "xx_next"] <- NA;  
			rdf[nk, "yy"] <- tdf_major[[nk]]; rdf[nk, "fill"] <- "(major)"; rdf[nk, "type"] <- "major";  
		 
		for(nj in names(tdf_minor)) { 
			njk <- paste(nk, nj, sep=more$fiscal_sep); 
			njk_next <- paste(more$next_year(nk), nj, sep=more$fiscal_sep); 
			 
		rdf[njk, "xx"] <- njk; rdf[njk, "xx_short"] <- nj; rdf[njk, "xx_next"] <- njk_next;  
			rdf[njk, "yy"] <- NA; rdf[njk, "fill"] <- nj; rdf[njk, "type"] <- "minor";  
		} 
		 
	} 
	 
	df_new <- df[df$fill_new == 1, ]; tdf_new <- lapply_split_sum(df_new, df_new$xx_fill_last); 
	for(njk in names(tdf_new)) { rdf[njk, "yy"] <- tdf_new[[njk]]; }	 
	 
	 
	df_old <- df[df$fill_new != 1, ]; tdf_old <- lapply_split_sum(df_old, df_old$xx_fill); 
	for(njk in names(tdf_old)) { rdf[njk, "yy_this"] <- tdf_old[[njk]]; } 
	 
	 
	for(k in 1:nrow(rdf)) {  
		nk <- rdf$xx_next[k]; vk <- rdf[nk, "yy_this"]; rdf[k, "yy_next"] <- vk;  
		yk <- rdf$yy[k]; 
		rdf[k, "yy_diff"] <- if( is.na(yk) ) { minus_na(rdf[k, "yy_next"], rdf[k, "yy_this"]); } else { yk; }  
	} 
	 
	return(rdf); 
	 
	sel <- sapply(rdf$xx_next, FUN=function(xx) { is.na(xx) | substr(xx, 1, 3) != "DEL" });	 
	df <- rdf[sel, ]; rdf <- NULL; 
	 
	df$x1 <- as.integer(factor(df$xx)) - more$box_width; 
	df$x2 <- df$x1 + 2 * more$box_width; 
		 
	s <- 0; 
	for(k in 1:nrow(df) ) { 
		if( df$type[k] == "major" ) { s <- df$yy[k]; } else { df[k, "y1"] <- s; s <- s + df$yy_diff[k]; df[k, "y2"] <- s;  } 
	} 
			 
	return(df); 
} 
minus_na <- function(a, b) { 
	if( is.na(a) | is.na(b) ) return(0); 
	return(a - b); 
} 
next_year_FY17_YTD22 <- function(nk) { 
	map <- list(FY17='FY18', FY18='FY19', FY19='FY20', FY20='FY21', FY21='DEL1', YTD21='YTD22', YTD22='DEL2'); 
	return(map[[nk]]); 
} 
last_year_FY17_YTD22 <- function(nk) { 
	map <- list(FY17='DEL16', FY18='FY17', FY19='FY18', FY20='FY19', FY21='FY20', LTM22='FY21', YTD22='YTD21', YTD21='DEL20'); 
	return(map[[nk]]); 
} 
draw_dyadic_trend_view <- function(df=rename(dataset, "xx", "yy", "fill", "panel")) { 
chart1 <- function(df, more=NULL) {     
    g <- ggplot(df); 
    g <- g + geom_bar(aes(x=xx, y=yy, fill=fill), stat="identity", show.legend=TRUE); 
    return(g); } 
chart2 <- function(df, more=NULL) {     
    g <- ggplot(df); 
    g <- g + geom_bar(aes(x=xx, y=yy, fill=fill), stat="identity", position="fill", show.legend=TRUE); 
    g <- g + geom_text(aes(x=xx, y=yy, label=fmt_c1(yy), group=fill), position=position_fill(vjust=0.5), show.legend=FALSE); 
    return(g); } 
    ldf <- list(A=chart1(df=df), B=chart2(df=df)); 
    g <- ggplot() + theme_void() + geom_array(grobs=ldf, ncol=2) + ggtitle('dyadic view of operational revenue'); 
    print(g); 
} 
draw_triadic_trend_view <- function(df=rename(dataset, "xx", "yy", "fill", "panel"), 
more=NULL, edit_more=NULL) { 
chart1 <- function(df, more=NULL) {     
    g <- ggplot(df); 
    g <- g + geom_bar(aes(x=xx, y=yy, fill=fill), stat="identity", show.legend=more$legend); 
    g <- g + geom_text(aes(x=xx, y=yy, label=more$fmt_text(yy), group=fill), position=position_stack(vjust=0.5), show.legend=FALSE); 
    return(more$fmt_chart(g)); } 
chart2 <- function(df, more=NULL) {     
	for(k in 1:nrow(df)) { df$yy_GRR[k] <- more$fmt_100(df$yy_GR[k]); } 
	 
    g <- ggplot(df); 
    g <- g + geom_bar(aes(x=xx, y=1, fill=fill), stat="identity", position="fill", show.legend=more$legend); 
    g <- g + geom_text(aes(x=xx, y=1, label=yy_GRR, group=fill), position=position_fill(vjust=0.5), show.legend=FALSE); 
    return(more$fmt_chart(g)); } 
     
     
chart3 <- function(df, more=NULL) {     
    g <- ggplot(df); 
    g <- g + geom_bar(aes(x=xx, y=1, fill=fill), stat="identity", position="fill", show.legend=more$legend); 
    g <- g + geom_text(aes(x=xx, y=1, label=more$fmt_100(yy_CT), group=fill), position=position_fill(vjust=0.5), show.legend=FALSE); 
    return(more$fmt_chart(g)); } 
chart4 <- function(df, more=NULL) { 
    g <- ggplot(df) + theme_void(); 
    g <- g + geom_bar(aes(x=fill, y=1, fill=fill), stat="identity", show.legend=FALSE) + coord_flip(); 
    g <- g + geom_text(aes(x=fill, y=0, label=fill, group=fill), hjust="inward", show.legend=FALSE); 
    return(g); } 
     
main <- function() { 
	more <- list(fmt_chart=fmt_chart, fmt_text=fmt_c1, fmt_100=fmt_100, legend=FALSE, last_year=last_year_FY17_YTD22, debug_table=FALSE, 
	   array_ncol=3, array_title='triadic trend view of operational revenue', xx_sep="/"); 
	    
	df <- add_GRCT_labels(df, more);    
		 
    ldf <- list(A=chart1(df=df, more=more), B=chart2(df=df, more=more), C=chart3(df=df, more=more), C=chart4(df, more=more)); 
    ncol <- rbind(c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4)); 
    g <- ggplot() + theme_void() + geom_front(arrangeGrob(grobs=ldf, layout_matrix=ncol)) + ggtitle(more$array_title); 
    if(more$debug_table) g <- g + geom_head(df, top=50); 
    print(g); } 
     
    main(); 
} 
last_year_FY17_YTD22 <- function(xk) { 
	map <- list(FY17='NA16', FY18='FY17', FY19='FY18', FY20='FY19', FY21='FY20', LTM22='FY21', YTD21='NA20', YTD22='YTD21'); 
	return(map[[xk]]); }     
add_GRCT_labels <- function(df, more) { 
	tdf <- lapply(split(df, df$xx), FUN=function(ddd) { sum(ddd$yy) }); 
	 
	row.names(df) <- paste(df$xx, df$fill, sep=more$xx_sep);    
	 
	for(k in 1:nrow(df)) { 
		xk <- as.character(df$xx[k]); 
		fk <- as.character(df$fill[k]); 
		 
		tk <- tdf[[xk]];  
		df$yy_foot[k] <- tk; df$yy_CT[k] <- df$yy[k] / tk; 
		 
		lk <- paste(more$last_year(xk), fk, sep=more$xx_sep); yy_last <- df[lk, "yy"]; 
		df$xx_last[k] <- lk; df$yy_last[k] <- yy_last;  
		df$yy_GR[k] <- if( is.na(yy_last) ) { NA } else { (df$yy[k] - df$yy_last[k])/df$yy_last[k]; } 
	} 
	 
	row.names(df) <- NULL; 
	 
	return(df); 
} 
