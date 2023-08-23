
## to draw waterfall chart with xx-yy-fill-sort order
source('https://raw.githubusercontent.com/tutrunghieu/minir/main/2308/sorted-waterfall.R');
colors <- list('major'='yellow', 'price'='#aa01dd', 'vol'='#01bb02', 'mix'='#4133cc');
main();


## to draw waterfall chart with xx-yy-fill-sort-pane order

source('https://raw.githubusercontent.com/tutrunghieu/minir/main/2308/sorted-waterfall.R');
source('https://raw.githubusercontent.com/tutrunghieu/minir/main/2308/waterfall-enum.R');
colors <- list('major'='yellow', 'price'='#8A8A8A', 'vol'='#A3A3A3', 'mix'='#C2C2C2');

df <- rename(dataset, "xx", "yy", "fill", "sort", "tile");
waterfall_enum(df);