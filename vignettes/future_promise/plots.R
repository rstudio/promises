library(ggplot2)

receive_point <- function(y) {
  geom_point(
    mapping = aes(shape = shape),
    data = data.frame(
      x = 0,
      y = y,
      shape = "receive"
    ),
    size = 4
  )
}
return_point <- function(x, y) {
  geom_point(
    mapping = aes(shape = shape),
    data = data.frame(
      x = x,
      y = y,
      shape = "return"
    ),
    size = 3
  )
}
working_line <- function(x, y) {
  geom_line(
    mapping = aes(group = y, linetype = linetype),
    data = data.frame(
      x = x,
      y = y,
      linetype = "working"
    ),
    size = 2
  )
}
waiting_line <- function(x, y, group = y) {
  geom_line(
    mapping = aes(group = group, linetype = linetype),
    data = data.frame(
      x = x,
      y = y,
      linetype = "waiting",
      group = group
    )
  )
}

save_image <- function(p, file, height = 4, width = 6, ...) {
  p <- p + theme(aspect.ratio = 100/150)
  ggsave(file, p, height = height, width = NA, ...)
}

route_type_guide <- function(
  future_values = NULL,
  plumber_values,
  promise_values = NULL
) {
  values <- c()
  values[plumber_values] <- "#2D9180" # fast
  labels <- "plumber"
  breaks <- plumber_values[1]

  if (!is.null(future_values)) {
    values[future_values] <- "#D77D49" # slow
    labels <- c("plumber + future", labels)
    breaks <- c(future_values[1], breaks)
  }
  if (!is.null(promise_values)) {
    values[promise_values] <- "#9437FF" # slow
    labels <- c("plumber + future_promise", labels)
    breaks <- c(promise_values[1], breaks)
  }
  scale_color_manual(
    name = "Route Type",
    values = values,
    labels = labels,
    breaks = breaks,
    guide = guide_legend(
      order = 1,
      override.aes = list(
        size = 1
      )
    )
  )
}

status_guide <- function(waiting = TRUE, promise = FALSE) {
  scale_linetype_manual(
    name = "Status",
    values = c(
      if (waiting) c(waiting = "dashed"),
      working = "solid"
    ),
    breaks = c(if (waiting) "waiting", "working"),
    labels = if (promise) c("Waiting in promise", "Working in future") else c(if (waiting) "Waiting", "Working"),
    guide = guide_legend(
      order = 3,
      override.aes = list(
        linetype = c(if (waiting) "dashed", "solid"),
        size = c(if (waiting) 0.5, 1.5)
      )
    )
  )
}

constants <-
  list(
    labs(
      x = "time (s)",
      y = NULL
    ),
    xlim(0, 20),
    scale_shape_manual(
      name = "Execution",
      values = c(
        "startend" = "X",
        "receive" = "|",
        "return" = "circle"
      ),
      breaks = c("receive", "startend", "return"),
      labels = c("Receive", "Process", "Respond"),
      guide = guide_legend(order = 2)
    ),
    # theme(aspect.ratio = 100 / 125),
    status_guide()
  )


p <-
  ggplot(mapping = aes(x = x, y = y)) +
  geom_point(
    data = data.frame(x = c(0, 0, 10, 10), y = c("a", "b", "b", "c")),
    color = "transparent"
  ) +
  scale_y_discrete(
    limits = rev(letters[1:3]),
    labels = c("/fast", "/slow", "/fast")
  ) +
  constants

save_image(p, "images/timing-blank.png")


p <-
  ggplot(mapping = aes(x = x, y = y, color = y)) +
  receive_point(letters[1:4]) +
  return_point(
    c(0, 10, 20, 20),
    letters[1:4]
  ) +
  working_line(
    x = c(0, 10, 10, 20),
    y = c("b", "b", "c", "c")
  ) +
  waiting_line(
      x = c(0, 10, 0, 20),
      y = c("c", "c", "d", "d")
  ) +
  route_type_guide(NULL, letters[1:4]) +
  scale_y_discrete(
    limits = rev(letters[1:4]),
    labels = c("/fast/4", "/slow/3", "/slow/2", "/fast/1")
  ) +
  constants

save_image(p, "images/timing-plumber.png", width = 5.5)


p <-
  ggplot(mapping = aes(x = x, y = y, color = y)) +
  receive_point(letters[1:4]) +
  return_point(
    c(0, 10, 10, 0),
    letters[1:4]
  ) +
  # just to make legend happy
  waiting_line(
    x = c(0, 0),
    y = c("b", "b")
  ) +
  working_line(
    x = c(0, 10, 0, 10),
    y = c("b", "b", "c", "c")
  ) +
  route_type_guide(c("b", "c"), c("a", "d")) +
  scale_y_discrete(
    limits = rev(letters[1:4]),
    labels = c("/fast/4", "/slow/3", "/slow/2", "/fast/1")
  ) +
  constants


save_image(p, "images/timing-plumber-future.png")




p <-
  ggplot(mapping = aes(x = x, y = y, color = y)) +
  receive_point(letters[2:6]) +
  return_point(
      x = c(10, 10, 20, 20, 0),
      y = letters[2:6]
  ) +
  working_line(
    x = c(0, 10, 0, 10, 10, 20, 10, 20),
    y = c("b", "b", "c", "c", "d", "d", "e", "e")
  ) +
  waiting_line(
    x = c(0, 10, 0, 10),
    y = c("d", "d", "e", "e")
  ) +
  route_type_guide(c("b", "c", "d", "e"), "f") +
  scale_y_discrete(
    limits = c("f", "e", "d", "c", "b"),
    labels = c("/fast/5", "/slow/4", "/slow/3", "/slow/2", "/slow/1")
  ) +
  constants


save_image(p, "images/timing-plumber-limitation.png")






future_constants <- list(
  scale_y_discrete(
    limits = rev(letters[1:7]),
    labels = c("/fast/7", "/slow/6", "/slow/5", "/slow/4", "/slow/3", "/slow/2", "/slow/1")
  ),
  constants[c(-2)],
  theme(
    legend.position = "right",
    # legend.title = element_text(size = 5)
  )
)

p <-
  ggplot(mapping = aes(x = x, y = y, color = y)) +
  receive_point(letters[1:7]) +
  return_point(
    c(20, 20, 20, 20, 20, 30, 30),
    c("g", "a", "b", "c", "d", "e", "f")
  ) +
  waiting_line(
    x = c(
      10, 20,
      10, 20,
      0, 10,# 20, 30,
      0, 10,# 20, 30,
      0, 20,
      0, 20,
      0, 20
    ),
    y = rep(letters[1:7], c(2, 2, 2, 2, 2, 2, 2)),
    group = c(
      "a2", "a2",
      "b2", "b2",
      "c1", "c1", #"c2", "c2",
      "d1", "d1", #"d2", "d2",
      "e1", "e1",
      "f1", "f1",
      "g1", "g1"
    )
  ) +
  working_line(
    x = c(0, 10, 0, 10, 10, 20, 10, 20, 20, 30, 20, 30),
    y = rep(letters[1:6], each = 2)
  ) +
  route_type_guide(letters[1:6], letters[7]) +
  future_constants


save_image(p, "images/timing-plumber-worker_full-future.png")


p <-
  ggplot(mapping = aes(x = x, y = y, color = y)) +
  # # start / end time
  # geom_point(
  #   mapping = aes(shape = shape),
  #   data = data.frame(
  #     x = c(0, 10, 0, 10, 10, 20, 10, 20, 20, 30, 20, 30, 0),
  #     y = c(rep(letters[1:6], each = 2), "g"),
  #     shape = "startend"
  #   ),
  #   size = 4
  # ) +
  receive_point(letters[1:7]) +
  return_point(
    c(0, 10, 10, 20, 20, 30, 30),
    c("g", "a", "b", "c", "d", "e", "f")
  ) +
  # wait time
  geom_line(
    mapping = aes(group = group, linetype = linetype),
    data = data.frame(
      x = c(
        0, 10,
        0, 10,
        0, 20,
        0, 20
      ),
      y = rep(c("c", "d", "e", "f"), each = 2),
      group = c(
        "c1", "c1",
        "d1", "d1",
        "e1", "e1",
        "f1", "f1"
      ),
      linetype = "waiting"
    )
  ) +
  # execution time
  working_line(
    x = c(0, 10, 0, 10, 10, 20, 10, 20, 20, 30, 20, 30),
    y = rep(letters[1:6], each = 2)
  ) +
  route_type_guide(promise_values = letters[1:6], plumber_values = letters[7]) +
  future_constants +
  status_guide(promise = TRUE)


save_image(p, "images/timing-plumber-worker_full-promise.png", width = 6.5)
