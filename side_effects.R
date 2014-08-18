library(magrittr)
devtools::install_github("renkun-ken/pipeR@0.4")
library(pipeR)

mtcars %T>%
  par(mfrow=c(2,2)) %>>%    # only for side effect (par() returns the arg list)
  (lm(mpg ~ cyl + wt, data = .)) %>>%
  plot()

#with TEE %T>%
mtcars %T>%
  par(mfrow=c(1,2)) %T>%
  plot(mpg ~ cyl, data = .) %T>%
  plot(mpg ~ wt, data = .) %>>%
  (lm(mpg ~ cyl + wt, data = .)) %>>%
  summary() %>>%
  (coefficients)

mtcars %>>%
  (~ par(mfrow=c(1,2)) ) %>>%
  (~ plot(mpg ~ cyl, data = .) ) %>>%
  (~ plot(mpg ~ wt, data = .) ) %>>%
  (lm(mpg ~ cyl + wt, data = .)) %>>%
  summary() %>>%
  (coefficients)


#think this will be useful for reference classes (R5)
install.packages('lme4')
library(lme4)

#borrow from lme4 vignette to test side effects operator
#str(sleepstudy)
#fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
sleepstudy %>>% 
  ( ~ str(.) ) %>>%  #note ( ~ str ) does not print str but still passes through
  #found this in the .Rnw but code is not in final vignette output  
  (~ 
    print(lattice::xyplot(Reaction ~ Days | Subject, ., aspect = "xy",
                    layout = c(9, 2), type = c("g", "p", "r"),
                    index.cond = function(x, y) coef(lm(y ~ x))[2],
                    xlab = "Days of sleep deprivation",
                    ylab = "Average reaction time (ms)",
                    as.table = TRUE))
  ) %>>%
  { lmer( Reaction ~ Days + ( Days | Subject ), . ) } %>>%
  ( ~ assign( "fm1", ., envir = .GlobalEnv ) ) %>>%
  #test some nested calls with the profile from vignette conclusion
  ( ~ profile( . ) %>>% { print(lattice::splom(.) ) } )

Pipe(sleepstudy)$
  .( ~ str(.) )$
  .( ~ 
     print(lattice::xyplot(Reaction ~ Days | Subject, ., aspect = "xy",
                           layout = c(9, 2), type = c("g", "p", "r"),
                           index.cond = function(x, y) coef(lm(y ~ x))[2],
                           xlab = "Days of sleep deprivation",
                           ylab = "Average reaction time (ms)",
                           as.table = TRUE))
  )$
  .( lmer( Reaction ~ Days + ( Days | Subject ), . ) )$
  .( ~ assign( "fm1", ., envir = .GlobalEnv ) )$
  .( ~ profile( . ) %>>% { print(lattice::splom(.)) } )[]


# formula module
#   parsedFormula <- lFormula(formula = Reaction ~ Days + (Days|Subject),
#                                data = sleepstudy)
# 
#   # objective function module
#   devianceFunction <- do.call(mkLmerDevfun, parsedFormula)
# 
#   # optimization module
#   optimizerOutput <- optimizeLmer(devianceFunction)
# 
#   # output module
#   mkMerMod( rho = environment(devianceFunction),
#                opt = optimizerOutput,
#                reTrms = parsedFormula$reTrms,
#                fr = parsedFormula$fr)

sleepstudy %>>%
  ( ~ print("# formula module")) %>>%
  { 
    lFormula (
      formula = Reaction ~ Days + (Days|Subject)
      , data = .
    )
  } %>>%
  ( ~ assign( "parsedFormula", ., envir = .GlobalEnv ) ) %>>%
  ( ~ cat( "test parsedFormula$frame == fm1@frame" ) )%>>%
  ( ~ testthat::is_identical_to(fm1@frame,parsedFormula$fr) ) %>>%
  ( ~ print( "optimization module" ) ) %>>%
  { do.call( mkLmerDevfun, . ) } %>>%
  ( ~ assign( "devianceFunction", ., envir = .GlobalEnv ) ) %>>%
  ( ~ print( "output module" ) ) %>>%
  optimizeLmer %>>%
  {
    mkMerMod (
      rho = environment( devianceFunction )
      ,opt = .
      ,reTrms = parsedFormula$reTrms,
      ,fr = parsedFormula$fr
    )
  }



# from timelyportfolio lme4 error bar post
# http://timelyportfolio.github.io/rCharts_errorbar/ucla_melogit.html
"http://www.ats.ucla.edu/stat/data/hdp.csv" %>>%
  read.csv %>>%
  within( {
    Married <- factor(Married, levels = 0:1, labels = c("no", "yes"))
    DID <- factor(DID)
    HID <- factor(HID)
  } ) %>>%
  {
    glmer(remission ~ Age + LengthofStay + FamilyHx + IL6 + CRP +
          CancerStage + Experience + (1 | DID) + (1 | HID),
          data = ., family = binomial, nAGQ=1)
  } %>>%  # show the dotplot as a reference
  (~
     print(lattice::dotplot(
       ranef(., which = "DID", postVar = TRUE),
       scales = list(y = list(alternating = 0))
     ))
  ) %>>%
  { ranef(object  = ., which = "DID", postVar = TRUE)$DID } %>>%
  {
    data.frame(
      "id" = rownames(.),  #this will be our x
      "intercept" = .[,1],            #this will be our y
      "se" = as.numeric(attr( ., "postVar" ))  #this will be our se
    )
  } %>>%  #had not thought of this use to add library
  (~ library(rCharts) ) %>>%  
  #rCharts good ref class reference for side effect helpfulness
  {
    setRefClass(
      "rChartsError"
      ,contains="rCharts"
      ,methods=list(
        initialize = function(){
          callSuper()
        }
        ,getPayload = function(chartId){
          list(chartParams = toJSON2(params), chartId = chartId, lib = basename(lib), liburl = LIB$url)
        }
      )
    )$new() %>>%
        (~ .$setLib("http://timelyportfolio.github.io/rCharts_errorbar") ) %>>%
        (~ .$setTemplate (
          script = "http://timelyportfolio.github.io/rCharts_errorbar/layouts/chart.html"
          ,chartDiv = "<div></div>"
        ) ) %>>%
        (~ .$set(
          data = get(".",parent.env(environment())),  #ugly but don't know better way
          height = 500,
          width = 1000,
          margin = list(top = 10, bottom = 10, right = 50, left = 100),
          x = "id",
          y = "intercept",
          radius = 2,
          sort = list( var = "intercept" ),
          whiskers = "#!function(d){return [d.intercept - 1.96 * d.se, d.intercept + 1.96 * d.se]}!#",
          tooltipLabels = c("id","intercept","se") 
        ))
  }





pdf("test.pdf")
data.frame( x = 1:10, y = 1:10 ) %>>%
  ( ~ plot( x = .[,"x"], y = .[,"y"], type = "b" ) ) %>>%
  ( ~ library(latticeExtra) ) %>>%
  ( ~ xyplot( y ~ x, data = ., type = c("p","l") ) %>>% print %>>%  ( ~ asTheEconomist(.) %>>% print ) ) %>>%
  ( ~ library(ggplot2) ) %>>% 
  ( ~ ggplot( ., aes( x = x, y = y) )  %>>% + geom_line() %>>% + geom_point() %>>% print )
dev.off()