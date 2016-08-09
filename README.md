# wrviz - create visualizations for water resources

Right now, this package only has one function: climate_heatmap. You can use this to create heatmaps for precipitation and temperature with either binary or continuous color scales.

Basic example:
```r
df <- expand.grid(temp=0:8,precip=seq(0.7,1.3,by=0.1))
df$rel <- seq(40,100,length=63)
climate_heatmap(df,"rel",80)
```

With a continuous, diverging scale:
```r
climate_heatmap(df,"reliability", binary = FALSE, metricCol = "rel")
```
You can also specify your own color scales:
```r
# please don't use these colors in an actual plot
climate_heatmap(df,"rel", colorScale = c("darkturquoise","salmon"))
```
Because `climate_heatmap` returns a ggplot object, you can make changes as needed after it's made (for example, changing units on the x axis):
```r
climate_heatmap(df,"reliability", binary = FALSE, metricCol = "rel") + 
  labs(x = expression("Temperature change (" * degree * F *")"))
```
