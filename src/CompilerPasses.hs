module CompilerPasses where

  {--
    When we think about our passes, we know that all passes have a
    Right, and we want this Right to be an Left Right or a
    value of the computation we've done. In addition, we have some
    state we'll be passing around. This has to be returned in case
    we've changed the state, but it also needs to be carried along
    in our functions. Think of CompilerState as an accumulator in
    a fold computation over a list, for example.
  --}

  -- Result by default is just a type alias for Either String a
  type Result a = Either String a

  -- we define a return type, or result type, for our compiler
  -- because we need to return information about a particular pass
  -- along with the result of the pass, which is often a transformation
  -- of the AST. You can think of this in some ways as an accumulator
  -- for a fold operation over an AST.
  data CompilerResult a b = CState a (Result b) deriving (Eq, Show)
