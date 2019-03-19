module Neuron where
-- includes input, bias, hidden, and output types

data Neuron = Basic [Int] | Imaginary [Char]
                deriving (Show, Eq)