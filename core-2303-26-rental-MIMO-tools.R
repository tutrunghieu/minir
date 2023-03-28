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
gt11 <- function(df, top=11) {  
	grid.table( head(df, top) );  
} 
ggplot_more <- function(df=NULL, more=NULL) { 
	ggplot(); 
} 
unique_vals <- function(...) { 
    vals <- as.character(unlist(list(...))); 
	voc <- c(); 
	for(vk in vals) voc[vk] <- 1;  
	voc <- names(voc); 
	 
	return( voc[order(voc)] ); 
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
fmt_z <- function(x) { x } 
fmt_100 <- function(x, s=100) { paste0(format(round(x*s, 1), nsmall=1, big.mark=","), '%'); } 
fmt_100b <- function(x, s=100) { format(round(x*s, 1), nsmall=1, big.mark=","); } 
fmt_c1 <- function(x, div=1) { format(round(x/div, 1), nsmall=1, big.mark=","); } 
fmt_c1_e3 <- function(x, div=1e3) { format(round(x/div, 1), nsmall=1, big.mark=","); } 
fmt_c1_e6 <- function(x, div=1e6) { format(round(x/div, 1), nsmall=1, big.mark=","); } 
fmt_c1_e9 <- function(x, div=1e9) { format(round(x/div, 1), nsmall=1, big.mark=","); } 
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
