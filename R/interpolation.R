cross_extrapolate <- function(x, y, new_x, curve_type = "linear", points = 2) {
  # x must be ordered
  stopifnot(!is.unsorted(x))

  # Only interpolate in x
  stopifnot(new_x > range(x, na.rm = T)[1] & new_x < range(x, na.rm = T)[2])
  stopifnot(length(x) == length(y))

  # have enough data points
  if (curve_type == "linear") {
    stopifnot(points >= 2)
  } else if (curve_type == "quadratic") {
    stopifnot(points >= 3)
  } else if (curve_type == "cubic") {
    stopifnot(points >= 4)
  }

  leftx <- x[x < new_x]
  stopifnot(length(leftx) >= points)
  lefty <- head(y, length(leftx))

  leftx <- tail(leftx, points)
  lefty <- tail(lefty, points)

  rightx <- x[x > new_x]
  stopifnot(length(rightx) >= points)
  righty <- tail(y, length(rightx))

  rightx <- head(rightx, points)
  righty <- head(righty, points)

  if (curve_type == "linear") {
    leftfit <- lm(lefty ~ leftx)
    rightfit <- lm(righty ~ rightx)

    left_est <- predict(leftfit, newdata = list(leftx = new_x))
    right_est <- predict(rightfit, newdata = list(rightx = new_x))
  } else if (curve_type == "quadratic") {
    leftx2 <- leftx^2
    rightx2 <- rightx^2
    leftfit <- lm(lefty ~ leftx + leftx2)
    rightfit <- lm(righty ~ rightx + rightx2)

    left_est <- predict(leftfit, newdata = list(leftx = new_x, leftx2 = new_x^2))
    right_est <- predict(rightfit, newdata = list(rightx = new_x, rightx2 = new_x^2))
  } else if (curve_type == "cubic") {
    leftx2 <- leftx^2
    leftx3 <- leftx^3
    rightx2 <- rightx^2
    rightx3 <- rightx^3
    leftfit <- lm(lefty ~ leftx + leftx2 + leftx3)
    rightfit <- lm(righty ~ rightx + rightx2 + rightx3)

    left_est <- predict(leftfit, newdata = list(leftx = new_x, leftx2 = new_x^2, leftx3 = new_x^3))
    right_est <- predict(rightfit, newdata = list(rightx = new_x, rightx2 = new_x^2, rightx3 = new_x^3))
  }

  if (new_x == max(leftx)) {
    return(left_est)
  } else if (new_x == min(rightx)) {
    return(right_est)
  } else {
    # new_x is between two points.
    left_dist <- abs(new_x - max(leftx, na.rm = T))
    right_dist <- abs(new_x - min(rightx, na.rm = T))

    return(weighted.mean(c(left_est, right_est), w = c(1 / left_dist, 1 / right_dist)))
  }
}
