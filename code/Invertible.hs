module Invertible where

import Algebra.Structures.Ring

data Ring r => Invertible r = Invertible [r] [r] [r]

