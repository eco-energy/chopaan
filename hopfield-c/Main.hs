{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Categorifier.C.Generate (writeCFiles)
import F (hopfieldCategorified)

-- Generates ./hopfield_step.c and ./hopfield_step.h in the current directory.
main :: IO ()
main = writeCFiles "." "hopfield_step" hopfieldCategorified
