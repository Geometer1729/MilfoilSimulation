module Dynamify (dynamify) where

dynamify::  (a -> Double ->  b) -> (b-> b -> Double) -> a -> Double ->  b
dynamify func norm input tol = rollingDynamify func norm tol tol input

rollingDynamify::(a -> Double ->  b) -> (b-> b -> Double) -> Double -> Double -> a -> b
rollingDynamify func norm tol try input = if dist < tol then next else rollingDynamify func norm tol (try/2) input
  where
    this = func input try
    next = func input (try/2)
    dist = norm this next
