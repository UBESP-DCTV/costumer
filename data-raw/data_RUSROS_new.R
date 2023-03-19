ROS3565_new <- list(
  name = "Random Oversampling with different ratio of undersampling",

  func = function(x, y) {
    if (!requireNamespace('unbalanced')) stop('package unbalanced needed.')

# message(paste('ROS_new original x', str(x)))
# message(paste('ROS_new original y', str(y)))
    dat    <- if (is.data.frame(x)) x else as.data.frame.simple_triplet_matrix(x)
    tab <- table(y)
# message(paste('ROS_new used dat', str(dat)))

    if (length(attr(tab, 'dimnames')) > 1 || length(tab) != 2) stop('Y has to be 2 levels')

    ord <- order(tab)
    levels(y)[ord] <- c(1, 0)

    k      <- floor((tab[ord][[2]] / tab[ord][[1]]) * 35/65) # k times original

    out    <- unbalanced::ubOver(X = dat, Y = y, k = k)
    levels(out$Y) <- names(tab)

# message(paste('ROS_new out', str(out, 1)))
    list(x = out[['X']], y = out[['Y']])
  },

  first = TRUE
)

# usethis::use_data(ROS3565_new, overwrite = TRUE)



RUS3565_new <- list(
  name = "Random Undersampling with different ratio of undersampling",

  func = function(x, y) {
    if (!requireNamespace('unbalanced')) stop('package unbalanced needed.')

    dat    <- if (is.data.frame(x)) x else as.data.frame.simple_triplet_matrix(x)
    tab <- table(y)

    if (length(attr(tab, 'dimnames')) > 1 || length(tab) != 2) stop('Y has to be 2 levels')

    ord <- order(tab)
    levels(y)[ord] <- c(1, 0)

    out    <- unbalanced::ubUnder(
      X      = dat,
      Y      = y,
      perc   = 35,
      method = 'percPos'
    )

    levels(out$Y) <- names(tab)

    list(x = out[['X']], y = out[['Y']])
  },

  first = TRUE
)

# usethis::use_data(RUS3565_new, overwrite = TRUE)




ROS5050_new <- list(
  name = "Random Oversampling with different ratio of undersampling",

  func = function(x, y) {
    if (!requireNamespace('unbalanced')) stop('package unbalanced needed.')

    # message(paste('ROS_new original x', str(x)))
    # message(paste('ROS_new original y', str(y)))
    dat    <- if (is.data.frame(x)) x else as.data.frame.simple_triplet_matrix(x)
    tab <- table(y)
    # message(paste('ROS_new used dat', str(dat)))

    if (length(attr(tab, 'dimnames')) > 1 || length(tab) != 2) stop('Y has to be 2 levels')

    ord <- order(tab)
    levels(y)[ord] <- c(1, 0)

    out    <- unbalanced::ubOver(X = dat, Y = y) # k = 0 means 50:50
    levels(out$Y) <- names(tab)

    # message(paste('ROS_new out', str(out, 1)))
    list(x = out[['X']], y = out[['Y']])
  },

  first = TRUE
)

# usethis::use_data(ROS5050_new, overwrite = TRUE)



RUS5050_new <- list(
  name = "Random Undersampling with different ratio of undersampling",

  func = function(x, y) {
    if (!requireNamespace('unbalanced')) stop('package unbalanced needed.')

    dat    <- if (is.data.frame(x)) x else as.data.frame.simple_triplet_matrix(x)
    tab <- table(y)

    if (length(attr(tab, 'dimnames')) > 1 || length(tab) != 2) stop('Y has to be 2 levels')

    ord <- order(tab)
    levels(y)[ord] <- c(1, 0)

    out    <- unbalanced::ubUnder(
      X      = dat,
      Y      = y,
      perc   = 50,
      method = 'percPos'
    )

    levels(out$Y) <- names(tab)

    list(x = out[['X']], y = out[['Y']])
  },

  first = TRUE
)

# usethis::use_data(RUS5050_new, overwrite = TRUE)

