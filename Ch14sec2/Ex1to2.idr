namespace Ex1
    data Access = LoggedOut | LoggedIn
    data PwdCheck = Correct | Incorrect
    data ShellCmd : (ty : Type) -> Access -> (ty -> Access) -> Type where 
        Password : String -> ShellCmd PwdCheck LoggedOut (\ck => case ck of
            Correct => LoggedIn
            Incorrect => LoggedOut)
        Logout : ShellCmd () LoggedIn (const LoggedOut)
        GetSecret : ShellCmd String LoggedIn (const LoggedIn)
        PutStr : String -> ShellCmd () state (const state)
        Pure : (res : ty) -> ShellCmd ty (state_fn res) state_fn 
        (>>=) : ShellCmd a state1 state2_fn -> ((res : a) -> ShellCmd b (state2_fn res) state3_fn) -> ShellCmd b state1 state3_fn



    session : ShellCmd () LoggedOut (const LoggedOut) 
    session = do 
        Correct <- Password "wurzel" | Incorrect => PutStr "Wrong password" 
        msg <- GetSecret
        PutStr ("Secret code: " ++ show msg ++ "\n")
        Logout

    -- sessionBad : ShellCmd () LoggedOut (const LoggedOut)
    -- sessionBad = do 
    --     Password "wurzel"
    --     msg <- GetSecret
    --     PutStr ("Secret code: " ++ show msg ++ "\n") 
    --     Logout

    -- noLogout : ShellCmd () LoggedOut (const LoggedOut) 
    -- noLogout = do
    --     Correct <- Password "wurzel" | Incorrect => PutStr "Wrong password" 
    --     msg <- GetSecret
    --     PutStr ("Secret code: " ++ show msg ++ "\n")

namespace Ex2
    VendState : Type
    VendState = (Nat, Nat)

    data Input = COIN
        | VEND
        | CHANGE
        | REFILL Nat

    data CoinResult = CoinInserted | CoinRejected

    data MachineCmd : (ty: Type) -> VendState -> (ty -> VendState) -> Type where
        InsertCoin : MachineCmd CoinResult (pounds, chocs) (\res => case res of
                        CoinInserted => (S pounds, chocs)
                        CoinRejected => (pounds, chocs))
        Vend       : MachineCmd () (S pounds, S chocs) (const (pounds, chocs))
        GetCoins   : MachineCmd () (pounds, chocs)     (const (Z, chocs))
        Refill     : (bars: Nat) ->
                     MachineCmd () (Z, chocs)          (const (Z, chocs + bars))
        Display    : String -> MachineCmd () state (const state)
        GetInput   : MachineCmd (Maybe Input) state (const state)
        Pure       : ty -> MachineCmd ty state (const state)
        (>>=)      : MachineCmd a state1 state2fn -> ((x:a) -> MachineCmd b (state2fn x) state3fn) -> MachineCmd b state1 state3fn

    data MachineIO : VendState -> Type where 
        Do : MachineCmd a state1 state2_fn -> ((x:a) -> Inf (MachineIO (state2_fn x))) -> MachineIO state1

    namespace MachineDo
        (>>=) : MachineCmd a state1 state2_fn -> ((x:a) -> Inf (MachineIO (state2_fn x))) -> MachineIO state1
        (>>=) = Do

    mutual
        vend : MachineIO (pounds, chocs)
        vend {pounds = S p} {chocs = S c}
            = do    Vend
                    Display "Enjoy!"
                    machineLoop
        vend {pounds = Z}
            = do    Display "Insert a coin"
                    machineLoop
        vend {chocs = Z} 
            = do    Display "Out of stock"
                    machineLoop

        refill : (num : Nat) -> MachineIO (pounds, chocs) 
        refill {pounds = Z} num
            = do    Refill num
                    machineLoop
        refill _ = do   Display "Can't refill: Coins in machine"
                        machineLoop

        machineLoop : MachineIO (pnds, chcs)
        machineLoop {pnds} {chcs} = do 
            Just x <- GetInput 
                | Nothing => do Display "Invalid input"
                                machineLoop
            case x of
                COIN => do  CoinInserted <- InsertCoin | CoinRejected => (machineLoop {chcs = chcs} {pnds = pnds})
                            machineLoop {pnds = S pnds} {chcs = chcs}
                VEND => vend
                CHANGE => do    GetCoins
                                Display "Change returned"
                                machineLoop
                REFILL num => refill num

