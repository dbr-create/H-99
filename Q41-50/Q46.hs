-- Q46, define predictaes and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 then create a function to print a predicate table

myAnd :: Bool -> Bool -> Bool
myAnd True True = True
myAnd _ _ = False

myOr :: Bool -> Bool -> Bool
myOr False False = False
myOr _ _ = True

myNand :: Bool -> Bool -> Bool
myNand True True = False
myNand _ _ = True

myNor :: Bool -> Bool -> Bool
myNor False False = True
myNor _ _ = False

myXor :: Bool -> Bool -> Bool
myXor True False = True
myXor False True = True
myXor _ _ = False

myImpl :: Bool -> Bool -> Bool
myImpl True False = False
myImpl _ _ = True

myEqu :: Bool -> Bool -> Bool
myEqu True True = True
myEqu False False = True
myEqu _ _ = False

data LogPred = And LogPred LogPred | Or LogPred LogPred | Nand LogPred LogPred | Nor LogPred LogPred | Xor LogPred LogPred | Impl LogPred LogPred | Equ LogPred LogPred | VarA | VarB

tableHelper :: Bool -> Bool -> LogPred -> Bool
tableHelper varA varB (And bool1 bool2) = myAnd (tableHelper varA varB bool1) (tableHelper varA varB bool2)
tableHelper varA varB (Or bool1 bool2) = myOr (tableHelper varA varB bool1) (tableHelper varA varB bool2)
tableHelper varA varB (Nand bool1 bool2) = myNand (tableHelper varA varB bool1) (tableHelper varA varB bool2)
tableHelper varA varB (Nor bool1 bool2) = myNor (tableHelper varA varB bool1) (tableHelper varA varB bool2)
tableHelper varA varB (Xor bool1 bool2) = myXor (tableHelper varA varB bool1) (tableHelper varA varB bool2)
tableHelper varA varB (Impl bool1 bool2) = myImpl (tableHelper varA varB bool1) (tableHelper varA varB bool2)
tableHelper varA varB (Equ bool1 bool2) = myEqu (tableHelper varA varB bool1) (tableHelper varA varB bool2)
tableHelper varA varB VarA = varA
tableHelper varA varB VarB = varB
