module Balance where

type State = (String, String)
newtype CheckPar a = CP (State -> (a, State))

app:: CheckPar a -> State -> (a, State)
app (CP cp) = cp

balance :: CheckPar Bool
balance = CP (\s -> balanceBrackets s)

balanceBrackets :: State -> (Bool, State)
balanceBrackets ([], [])                              = (True, ([], []))
balanceBrackets (left, [])                            = (False, (left, []))
balanceBrackets ([], r:rs)    | r == ']' || r == ')'  = (False, ([], r:rs))
                            | r == '[' || r == '('    = balanceBrackets ([r], rs)
                            | otherwise               = balanceBrackets ([], rs)
balanceBrackets (left, r:rs)  | r == '[' || r == '('  = balanceBrackets (left ++ [r], rs)
                            | (left !! (length left - 1) == '[' && r == ']') || (left !! (length left - 1) == '(' && r == ')') = balanceBrackets (take (length left - 1) left, rs)
                            | (left !! (length left - 1) == '[' && r == ')') || (left !! (length left - 1) == '(' && r == ']') = (False, (left, r:rs))
                            | otherwise = balanceBrackets (left, rs)