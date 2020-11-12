# funCAD
Functional [cadCAD](https://github.com/cadCAD-org/cadCAD)

```haskell
data State = State {
    _timestep :: Int,
    _value    :: Float
} deriving (Show)
makeLenses ''State

type StateHistory = [State]

initialState :: State
initialState = State 0 1

stateHistory :: [State]
stateHistory = [initialState]

updateTimestep :: State -> State
updateTimestep state = set timestep ((view timestep state) + 1) state

u1 :: State -> State
u1 state = set value ((view value state) + 1) state

model :: SF State State
model = arr updateTimestep
        >>^ u1

simulation :: State -> SF () State
simulation state = arr (\_ -> state) >>> model
```

# Tools

* [Arrows](https://en.wikibooks.org/wiki/Haskell/Understanding_arrows)
* [Yampa](https://wiki.haskell.org/Yampa)
* [Functional Reactive Programming with Yampa](https://tuttlem.github.io/2014/07/31/functional-reactive-programming-with-yampa.html)

## [Arrows: A General Interface to Computation](https://www.haskell.org/arrows/)

![Arrows](https://upload.wikimedia.org/wikipedia/commons/c/cf/ArrowsConveyors_bind2.png)

## Yampa signal functions
![Yampa](https://wiki.haskell.org/wikiupload/c/cb/Yampa_signal_functions.png)
