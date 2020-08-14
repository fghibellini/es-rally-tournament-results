library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)
library(gridExtra)
library(grid)

# plot counter (for filename)
i <- 0

# dataframe
df <- read.csv(file="./data.csv", sep=',', header=FALSE, quote='"', col.names=c("track", "var", "val"))

# min/median/max vars
{
  vars <- list(
   "op_metrics.0.throughput.",
   "total_time_per_shard.",
   "indexing_throttle_time_per_shard.",
   "refresh_time_per_shard.",
   "flush_time_per_shard.",
   "merge_time_per_shard.",
   "merge_throttle_time_per_shard."
  )

  lapply(vars, function(v) {
    # throughput
    tp <- df %>% filter(startsWith(as.character(var), v))

    tpw <- spread(tp, var, val)

    g <- ggplot(tpw, aes_string(
      x = "track",
      y = paste(v, "median", sep=""),
      ymin = paste(v, "min", sep=""),
      ymax = paste(v, "max", sep=""),
      fill = "track"
    )) + geom_boxplot() + geom_crossbar() + ylim(0, NA)

    #list(label = v, plot = g)
    ggsave(file=paste("plots/plot-", i, ".png", sep=""), plot=g)
    i <<- i+1
  })
}

# percentile vars
{
  vars <- list(
   "op_metrics.0.latency.",
   "op_metrics.0.service_time.",
   "op_metrics.0.processing_time."
  )

  lapply(vars, function(v) {
    # throughput
    tp <- df %>% filter(startsWith(as.character(var), v))

    tpw <- spread(tp, var, val)

    g <- (
        ggplot(tpw, aes_string(
          x = "track",
          y = paste(v, "50_0", sep=""),
          label = paste(v, "50_0", sep="")
          #ymin = paste(v, "min", sep=""),
          #ymin = paste(v, "50_0", sep=""),
          #lower = paste(v, "50_0", sep=""),
          #upper = paste(v, "90_0", sep=""),
          #ymax = paste(v, "99_0", sep="")
          #fill = "track"
        ))
      + geom_segment(aes_string(
          x = "track",
          y = paste(v, "50_0", sep=""),
          xend = "track",
          yend = paste(v, "100_0", sep=""),
          size = 2
        ), colour= "#FFCCCCFF")
      + geom_segment(aes_string(
          x = "track",
          y = paste(v, "50_0", sep=""),
          xend = "track",
          yend = paste(v, "99_99", sep=""),
          size = 2
        ), colour= "#FFAAAAFF")
      + geom_segment(aes_string(
          x = "track",
          y = paste(v, "50_0", sep=""),
          xend = "track",
          yend = paste(v, "99_9", sep=""),
          size = 2
        ), colour= "#FF8888FF")
      + geom_segment(aes_string(
          x = "track",
          y = paste(v, "50_0", sep=""),
          xend = "track",
          yend = paste(v, "99_0", sep=""),
          size = 2
        ), colour= "#FF0000FF")
      + geom_point(size = 2, shape = 18, colour= "#000000FF")
      + geom_point(aes_string(
          x = "track",
          y = paste(v, "100_0", sep="")
        ), size = 2, shape =18, colour= "#000000FF")
      + geom_point(aes_string(
          x = "track",
          y = paste(v, "99_99", sep="")
        ), size = 2, shape =18, colour= "#000000FF")
      + geom_point(aes_string(
          x = "track",
          y = paste(v, "99_9", sep="")
        ), size = 2, shape =18, colour= "#000000FF")
      + geom_point(aes_string(
          x = "track",
          y = paste(v, "99_0", sep="")
        ), size = 2, shape =18, colour= "#000000FF")
      #+ geom_text(hjust = "right")
      + ylim(0, NA)
    )

    #tbl <- tableGrob(tpw)

    #combined <- grid.arrange(g, tbl, nrow=2)

    #list(label = v, plot = g)
    ggsave(file=paste("plots/plot-", i, ".png", sep=""), plot=g)
    i <<- i+1
  })
}

# single value vars
{
  vars <- list(
   "op_metrics.0.error_rate",
   "total_time",
   "merge_time",
   "merge_count",
   "refresh_time",
   "refresh_count",
   "flush_time",
   "flush_count",
   "merge_throttle_time",
   "young_gc_time",
   "old_gc_time",
   "memory_doc_values",
   "memory_terms",
   "memory_norms",
   "memory_points",
   "memory_segments",
   "memory_stored_fields",
   "store_size",
   "translog_size",
   "segment_count"
  )

  lapply(vars, function(v) {
    tp <- df %>% filter(as.character(var) == v)

    tpw <- spread(tp, var, val)

    g <- ggplot(tpw, aes_string(
      x = "track",
      y = v,
      fill = "track"
    )) + geom_bar(stat = "identity") + ylim(0, NA)

    #list(label = v, plot = g)
    ggsave(file=paste("plots/plot-", i, ".png", sep=""), plot=g)
    i <<- i+1
  })
}

#do.call(ggarrange, c(
#  #lapply(plots, function(x) { x[2] }),
#  plots,
#  list(
#    common.legend = TRUE,
#    legend = "bottom"
#    # labels = lapply(plots, function(x) x["label"])
#  )
#))
#
##dev.off()
#ggsave("plot.png")
