parameters <- c(a = -5/3, b = -10, c = 28)
state <- c(X = 1, Y = 1, Z = 1)

Lorenz <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    # rate of change
    dX <- a * X + Y * Z
    dY <- b * (Y - Z)
    dZ <- -X * Y + c * Y - Z
    
    # return the rate of change
    list(c(dX, dY, dZ))
  })
}

times <- seq(0, 100, by = 0.01)

# integrate the ODE
out <- ode(y = state, times = times, func = Lorenz, parms = parameters)

out <- as.data.frame(out)
qplot(pull(out, X), pull(out, Y), size = I(0.1))
qplot(pull(out, X), pull(out, Z), size = I(0.1))
qplot(pull(out, Y), pull(out, Z), size = I(0.1))
qplot(pull(out, time), pull(out, X), size = I(0.1))
qplot(pull(out, time), pull(out, Y), size = I(0.1))
qplot(pull(out, time), pull(out, Z), size = I(0.1))

