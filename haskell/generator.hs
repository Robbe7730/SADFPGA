import Text.Regex
import Text.Regex.Base
import Data.Char
import Data.List

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)
data Direction = L | R deriving (Show)
type Directions = [Direction]

main = do
        putStrLn "Choose language: Verilog, abstract"
        language <- getLine
        putStrLn "Input proposition"
        input <- getLine
        let abst = translate input
        if language == "Verilog"
          then
            putStrLn (abstrToVerilog abst)
          else
            if language == "abstract"
              then
                putStrLn abst
              else
                putStrLn "Language not implemented"

abstrToVerilog :: String -> String
abstrToVerilog "" = ""
abstrToVerilog abst = treeToVerilog (strToTree abst)

treeToVerilog :: Tree Char -> String
treeToVerilog Empty = ""
treeToVerilog (Node x Empty Empty) = charToVerilog x
treeToVerilog (Node '>' left right) = "~(" ++ treeToVerilog left ++ ") | " ++ treeToVerilog right
treeToVerilog (Node x left right) = "(" ++ treeToVerilog left ++ ")" ++ charToVerilog x ++ "(" ++ treeToVerilog right ++ ")"

charToVerilog :: Char -> String
charToVerilog '&' = " & "
charToVerilog '/' = " | "
charToVerilog '!' = " ~ "
charToVerilog '=' = " ^~ "
charToVerilog '#' = " ^ "
charToVerilog '(' = "("
charToVerilog ')' = ")"
charToVerilog x = "reg_in[" ++ [x] ++ "]"

-- abstrToVerilog :: String -> String
-- abstrToVerilog "" = ""
-- abstrToVerilog abst = replaceUsingRegex (convImplies abst) opList
--         where allVars = getVariables abst
--               uniqueVars = nub allVars
--               finalVars = [[var !! (length var - i) |i <- [1..length var]] | var <- uniqueVars, var /= []]
--               varsList = [(mkRegex c, "reg_in[" ++ c ++ "]") | c <- finalVars]
--               andOp = (mkRegex "&", " & ")
--               orOp = (mkRegex "/", " | ")
--               notOp = (mkRegex "!", " ~")
--               equalsOp = (mkRegex "=", " ^~ ")
--               nEqualsOp = (mkRegex "#", " ^ ")
--               opList = andOp:orOp:notOp:equalsOp:nEqualsOp:varsList

strToTree :: String -> Tree Char
strToTree "" = Empty
strToTree str = Node (str !! sepIdx) (strToTree ll) (strToTree rl)
        where sepIdx = findSeparator (removeBetweenBrackets str)
              ll = remOuterBrackets [str !! i | i <- [0..sepIdx-1]]
              rl = remOuterBrackets [str !! i | i <- [sepIdx+1..length str - 1]]

level :: String -> [Integer]
level = scanl (\acc c -> acc + (if c=='(' then 1 else if c==')' then -1 else 0)) 0

remOuterBrackets :: String -> String
remOuterBrackets [] = []
remOuterBrackets [x] = [x]
remOuterBrackets str = if minimum (tail (init (level str))) == 0 then str else tail (init str)

removeBetweenBrackets str = [if level str !!i + level str !!(i+1) == 0 then str !! i else 'X'| i <- [0..length str - 1]]

findSeparator :: String -> Int
findSeparator [] = 0
findSeparator [x] = 0
findSeparator [x,y] = 0
findSeparator str = head [i | j <- "#=>/&!", i <- [0..length str-1], (str !! i) == j]

translate :: String -> String
translate [] = []
translate str = do
        let spacedBracketsStr = spaceBrackets str
        let translOperatorsStr = removeMultiChar spacedBracketsStr
        let noSpacesStr = filter (/=' ') translOperatorsStr
        prepVariables noSpacesStr

spaceBrackets :: String -> String
spaceBrackets str = subRegex (mkRegexWithOpts "\\)" False True) openBrackets " ) "
        where openBrackets = subRegex (mkRegexWithOpts "\\(" False True) str " ( "

removeMultiChar :: String -> String
removeMultiChar str = replaceUsingRegex str [(orReg, " / "), (notReg, " ! "), (andReg, " & "), (impReg, " > "), (equReg, " = "), (nEquReg, " # ")]
        where andReg = mkRegexWithOpts "( and |˄)" False True :: Regex
              orReg = mkRegexWithOpts "( or |˅)" False True :: Regex
              impReg = mkRegexWithOpts "( implies |=>|⇒)" False True :: Regex
              equReg = mkRegexWithOpts "( ((is )*equivalent( with)*) | ((is )*equal( to)*) )" False True :: Regex
              nEquReg = mkRegexWithOpts "( ((is )*not equivalent( with)*) | ((is )*not equal( to)*)) " False True :: Regex
              notReg = mkRegexWithOpts "(^not |( not )|~)" False True :: Regex

replaceUsingRegex :: String -> [(Regex, String)] -> String
replaceUsingRegex "" _ = ""
replaceUsingRegex str [] = str
replaceUsingRegex str opList = foldr (\t accStr -> subRegex (fst t) accStr (snd t)) str opList

prepVariables :: String -> String
prepVariables [] = ""
prepVariables str = replaceUsingRegex str opList
        where allVars = getVariables str
              uniqueVars = nub allVars
              finalVars = [[var !! (length var - i) |i <- [1..length var]] | var <- uniqueVars, var /= []]
              opList = [(mkRegex (finalVars !! i) :: Regex, [intToDigit i]) | i <- [0..length finalVars - 1]]

getVariables :: String -> [String]
getVariables = foldl helper [[]]
        where helper [[]] x = if x `elem` "/!&>=#()" then [[]] else [[x]]
              helper (full@(first:rest)) x = if x `elem` "/!&>=#()" then []:full else (x:first):rest
