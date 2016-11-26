## ----setup---------------------------------------------------------------
library(reda) # attach package
data(simuDat) # attach sample dataset

## ----data----------------------------------------------------------------
head(simuDat)
str(simuDat)

## ----const---------------------------------------------------------------
(constFit <- rateReg(Survr(ID, time, event) ~ group + x1, data = simuDat))

## ----twoPieces-----------------------------------------------------------
# two pieces' constant rate function
(twoPiecesFit <- rateReg(Survr(ID, time, event) ~ group + x1, df = 2,
                         data = simuDat, subset = ID %in% 1:50))

## ----sixPieces-----------------------------------------------------------
## df = 2 will be neglected since knots are explicitly specified
(piecesFit <- rateReg(Survr(ID, time, event) ~ group + x1, df = 2,
                      data = simuDat, subset = ID %in% 1:50,
                      knots = seq(from = 28, to = 140, by = 28)))

## ----spline--------------------------------------------------------------
## internal knots are set as 33% and 67% quantiles of time variable
(splineFit <- rateReg(Survr(ID, time, event) ~ group + x1,
                      data = simuDat, subset = ID %in% 1:50,
                      df = 6, degree = 3, spline = "mSplines"))
## or internal knots are expicitly specified
(splineFit <- rateReg(Survr(ID, time, event) ~ group + x1,
                      data = simuDat, subset = ID %in% 1:50,
                      spline = "bSplines", degree = 3L, knots = c(56, 112)))

## ----summary-------------------------------------------------------------
summary(constFit)
summary(piecesFit, showCall = FALSE)
summary(splineFit, showCall = FALSE, showKnots = FALSE)

## ----est-----------------------------------------------------------------
## point estimates of covariate coefficients
coef(splineFit)
## confidence interval for covariate coefficients
confint(splineFit, level = 0.95)
## estimated coefficients of baseline rate function
baseRate(splineFit)

## ----modelSelect---------------------------------------------------------
AIC(constFit, piecesFit, splineFit)
BIC(constFit, piecesFit, splineFit)

## ----sampleMcf-----------------------------------------------------------
## overall sample MCF for valve-seat data in Nelson (1995)
valveMcf <- mcf(Survr(ID, Days, No.) ~ 1, data = valveSeats)

## sample MCF for different groups
simuMcf <- mcf(Survr(ID, time, event) ~ group + gender,
               data = simuDat, subset = ID %in% 1 : 50, logConfInt = FALSE)

## ----plot:sampleMcf, fig.height = 5, fig.width = 7-----------------------
## Example 1. valve-seat data
plot(valveMcf, conf.int = TRUE, mark.time = TRUE) + ggplot2::xlab("Days")

## Example 2. sample simulated data
## plot after creating customized levels in legend
levs <- with(simuDat, expand.grid(levels(group), levels(gender)))
levs <- do.call(paste, c(as.list(levs), sep = " & "))
plot(simuMcf, conf.int = TRUE, lty = 1 : 4, legendName = "Treatment & Gender",
     legendLevels = levs) + ggplot2::xlab("Days")

## ----piecesMcf, fig.height = 5, fig.width = 7----------------------------
piecesMcf <- mcf(piecesFit)
plot(piecesMcf, conf.int = TRUE, col = "blueviolet") + ggplot2::xlab("Days")

## ----splineMcf, fig.height = 5, fig.width = 7----------------------------
newDat <- data.frame(x1 = rep(0, 2), group = c("Treat", "Contr"))
estmcf <- mcf(splineFit, newdata = newDat, groupName = "Group",
              groupLevels = c("Treatment", "Control"))
plot(estmcf, conf.int = TRUE, col = c("royalblue", "red"), lty = c(1, 5)) +
    ggplot2::ggtitle("Control vs. Treatment") + ggplot2::xlab("Days")

