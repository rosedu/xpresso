module IDraw where

data Component = Component {
    cType :: String,
    cInputs :: [String],
    cOutputs :: [String]
    } deriving (Eq, Show) 
