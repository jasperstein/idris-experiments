import Data.Vect
data StackCmd : Type -> Nat -> Nat -> Type where 
    Push : Integer -> StackCmd () height (S height) 
    Pop : StackCmd Integer (S height) height
    Top : StackCmd Integer (S height) (S height)
    
    GetStr : StackCmd String height height 
    PutStr : String -> StackCmd () height height

    Pure : ty -> StackCmd ty height height 
    (>>=) : StackCmd a height1 height2 -> (a -> StackCmd b height2 height3) -> StackCmd b height1 height3

doAdd : StackCmd () (S (S height)) (S height) 
doAdd = do val1 <- Pop
           val2 <- Pop
           Push (val1 + val2)

subtract : StackCmd () (S (S height)) (S height)
subtract = do op1 <- Pop
              op2 <- Pop
              Push (op2 - op1)

multiply : StackCmd () (S (S height)) (S height)
multiply = do op1 <- Pop
              op2 <- Pop
              Push (op1 * op2)

negate : StackCmd () (S height) (S height)
negate = do op1 <- Pop
            Push (-op1)

discard : StackCmd Integer (S height) height
discard = Pop

duplicate : StackCmd Integer (S height) (S (S height))
duplicate = do op <- Top
               Push op
               Pure op


runStack : (stk : Vect inHeight Integer) -> StackCmd ty inHeight outHeight -> IO (ty, Vect outHeight Integer)
runStack stk (Push val) = pure ((), val :: stk)
runStack (val :: stk) Pop = pure (val, stk) 
runStack (val :: stk) Top = pure (val, val :: stk) 
runStack stk GetStr = do x <- getLine
                         pure (x, stk)
runStack stk (PutStr x) = do putStr x
                             pure ((), stk)
runStack stk (Pure x) = pure (x, stk)
runStack stk (x >>= f) = do (x', newStk) <- runStack stk x 
                            runStack newStk (f x')



data StackIO : Nat -> Type where
    Do : StackCmd a height1 height2 -> (a -> Inf (StackIO height2)) -> StackIO height1

namespace StackDo
    (>>=) : StackCmd a height1 height2 -> (a -> Inf (StackIO height2)) -> StackIO height1 
    (>>=) = Do


data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

run : Fuel -> Vect height Integer -> StackIO height -> IO () 
run (More fuel) stk (Do c f) = do (res, newStk) <- runStack stk c
                                  run fuel newStk (f res)
run Dry stk p = pure ()


data StkInput = Number Integer
              | Add
              | Subtract
              | Multiply
              | Negate
              | Discard
              | Duplicate

strToInput : String -> Maybe StkInput
strToInput "" = Nothing
strToInput "add" = Just Add
strToInput "subtract" = Just Subtract
strToInput "multiply" = Just Multiply
strToInput "negate" = Just Negate
strToInput "discard" = Just Discard
strToInput "duplicate" = Just Duplicate
strToInput x = if all isDigit (unpack x)
               then Just (Number (cast x))
               else Nothing


mutual
  tryCmd2 : ({inH: Nat} -> StackCmd () (S (S inH)) (S inH)) -> StackIO height
  tryCmd2 cmd {height = (S (S h))}
    = do cmd
         result <- Top
         PutStr (show result ++ "\n")
         stackCalc
  tryCmd2 cmd
    = do PutStr "Fewer than two items on the stack\n"
         stackCalc
  
  tryNegate : StackIO height
  tryNegate {height = (S h)}
    = do negate
         result <- Top
         PutStr (show result ++ "\n")
         stackCalc
  tryNegate
    = do PutStr "No items on the stack\n"
         stackCalc

  tryDiscard : StackIO height
  tryDiscard {height = (S h)}
    = do result <- discard
         PutStr ("Discarded " ++ show result ++ "\n")
         stackCalc
  tryDiscard
    = do PutStr "No items on the stack\n"
         stackCalc

  tryDuplicate : StackIO height
  tryDuplicate {height = (S h)}
    = do result <- duplicate
         PutStr ("Duplicated " ++ show result ++ "\n")
         stackCalc
  tryDuplicate
    = do PutStr "No items on the stack\n"
         stackCalc
            
            

  stackCalc : StackIO height
  stackCalc = do PutStr "> "
                 input <- GetStr
                 case strToInput input of
                    Nothing => do PutStr "Invalid input\n"
                                  stackCalc
                    Just (Number x) => do Push x
                                          stackCalc
                    Just Add => tryCmd2 doAdd
                    Just Subtract => tryCmd2 subtract
                    Just Multiply => tryCmd2 multiply
                    Just Negate => tryNegate
                    Just Discard => tryDiscard
                    Just Duplicate => tryDuplicate



main : IO ()
main = run forever [] stackCalc