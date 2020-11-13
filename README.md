# funCAD
Functional [cadCAD](https://github.com/cadCAD-org/cadCAD)

This is still a WIP and doesn't do much. For the purpose of learning about Haskell, category theory, generalized dynamical systems modelling and simulation, in the context of cadCAD.

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

updateTimestep :: SF State State
updateTimestep =
    increment &&& id
    ^>> merge
    where
        increment = (+ 1) . (view timestep)
        merge = arr (uncurry (set timestep))

u1 :: SF State State
u1 =
    update &&& id
    ^>> merge
    where
        update = (+ 1) . (view value)
        merge = arr (uncurry (set value))

model :: SF State State
model = u1

simulation :: SF State State
simulation = updateTimestep >>> model
```

# Tools

* [Arrows](https://en.wikibooks.org/wiki/Haskell/Understanding_arrows)
* [Yampa](https://wiki.haskell.org/Yampa)
* [Functional Reactive Programming with Yampa](https://tuttlem.github.io/2014/07/31/functional-reactive-programming-with-yampa.html)

## [Arrows: A General Interface to Computation](https://www.haskell.org/arrows/)

![Arrows](https://upload.wikimedia.org/wikipedia/commons/c/cf/ArrowsConveyors_bind2.png)

## Yampa signal functions
![Yampa](https://wiki.haskell.org/wikiupload/c/cb/Yampa_signal_functions.png)
