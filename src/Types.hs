module Types (Config) where

type PolicyFunction = Int -> Int
data Policy = Policy { signal :: String, p_function :: PolicyFunction }

data State = State { key :: String, value :: Int }
type StateUpdate = State  -> State

data Block = Block { policies :: [Policy], variables :: [StateUpdate] }
type Structure = [[Policy] -> [StateUpdate] -> Block]

type States = [String]
type Params = [Float]

-- data Node = Policy | StateUpdate

data Config = Config { runs :: Int }

-- run :: Config -> Simulation -> SimulationResults
-- run config simulation = Configs runs

-- createModel :: States -> Params -> Structure -> Model
-- createModel states params structure = 

data SimulationResults = SimulationResults { result :: [Int] }
-- type Simulation = Model -> SimulationResults
