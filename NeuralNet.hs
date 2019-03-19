module NeuralNet where
import Neuron

data Mode = FeedForward | Backprop | Recurrent | Convolutional | Display
                deriving (Enum, Show, Eq)

data NeuralNet = NNet [Neuron] Mode | Inactive [Neuron]     -- feed forward NN
                deriving (Show, Eq)


-- generates neural network 
net_init :: IO()
net_init =  do 
    
    
    return ()


setMode :: NeuralNet -> Mode -> NeuralNet
setMode _ _ = NNet [] Backprop