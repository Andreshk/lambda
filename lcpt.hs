module LCPT where
import Lambda(Lambda(..), pretty, i, k, ks, s, w, om, y, j,
              valid, beta, betaStep, betaSteps, betaSteps_)
import NLambda
import Numerals
import W

main = putStrLn $
  "This module serves as to unify the exported functionality \n\
  \of the other modules. For example usage, see README.md"
