namespace Ex1

    data DoorState = DoorClosed | DoorOpen
    data DoorCmd : Type -> DoorState -> DoorState -> Type where
        Open : DoorCmd     () DoorClosed DoorOpen
        Close : DoorCmd    () DoorOpen   DoorClosed
        RingBell : DoorCmd () doorstate  doorstate
        Pure : ty -> DoorCmd ty state state
        (>>=) : DoorCmd a state1 state2 -> (a -> DoorCmd b state2 state3) -> DoorCmd b state1 state3

    doorProg: DoorCmd () DoorClosed DoorClosed
    doorProg = do RingBell
                  Open
                  RingBell
                  Close

namespace Ex2 
    data GuessCmd : Type -> Nat -> Nat -> Type where
        Try : Integer -> GuessCmd Ordering (S n) n
        Pure : ty -> GuessCmd ty state state 
        (>>=) : GuessCmd a state1 state2 -> (a -> GuessCmd b state2 state3) -> GuessCmd b state1 state3

    threeGuesses: GuessCmd () 3 0
    threeGuesses = do Try 10
                      Try 20
                      Try 15
                      Pure ()

    -- noGuesses: GuessCmd () 0 0
    -- noGuesses = do Try 10
                --    Pure ()
    
namespace Ex3
    data Matter = Solid | Liquid | Gas

    data MatterCmd: Type -> Matter -> Matter -> Type where
         Pure: ty -> MatterCmd ty matter matter
         (>>=): MatterCmd a matter1 matter2 -> (a -> MatterCmd b matter2 matter3) -> MatterCmd b matter1 matter3
         Melt: MatterCmd () Solid Liquid
         Boil: MatterCmd () Liquid Gas
         Condense: MatterCmd () Gas Liquid
         Freeze: MatterCmd () Liquid Solid

    iceSteam : MatterCmd () Solid Gas
    iceSteam = (do Melt
                   Boil)

    steamIce : MatterCmd () Gas Solid
    steamIce = (do Condense
                   Freeze)

    -- overMelt : MatterCmd () Solid Gas
    -- overMelt = do Melt
    --               Melt