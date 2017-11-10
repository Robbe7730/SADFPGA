import Text.Regex
import Text.Regex.Base
import Data.Char
import Data.List

main = do
        putStrLn "Choose language: FPGA, abstract"
        language <- getLine
        putStrLn "Input proposition"
        input <- getLine
        let abst = translate input
        if language == "FPGA"
          then
            putStrLn (abstrToFPGA abst)
          else
            if language == "abstract"
              then
                putStrLn abst
              else
                putStrLn "No such language"

abstrToFPGA :: String -> String
abstrToFPGA "" = ""
abstrToFPGA abst = replaceUsingRegex abst opList
        where allVars = getVariables abst
              uniqueVars = nub allVars
              finalVars = [[(var !! (length var - i)) |i <- [1..(length var)]] | var <- uniqueVars, var /= []]
              varsList = [(mkRegex c, "reg_in[" ++ c ++ "]") | c <- finalVars]
              andOp = (mkRegex "&", " & ")
              orOp = (mkRegex "/", " | ")
              notOp = (mkRegex "!", " ~")
              impliesOp = (mkRegex ">", " WIP ")
              equalsOp = (mkRegex "=", " ^~ ")
              nEqualsOp = (mkRegex "#", " ^ ")
              opList = andOp:orOp:notOp:impliesOp:equalsOp:nEqualsOp:varsList

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
              equReg = mkRegexWithOpts "( ((is )*equivalent( with)*)|((is )*equal( to)*))" False True :: Regex
              nEquReg = mkRegexWithOpts "( ((is )*not equivalent( with)*)|((is )*not equal( to)*))" False True :: Regex
              notReg = mkRegexWithOpts "(^not |( not )|~)" False True :: Regex

replaceUsingRegex :: String -> [(Regex, String)] -> String
replaceUsingRegex "" _ = ""
replaceUsingRegex str [] = str
replaceUsingRegex str opList = foldr (\t -> \accStr -> subRegex (fst t) accStr (snd t)) str opList

prepVariables :: String -> String
prepVariables [] = ""
prepVariables str = replaceUsingRegex str opList
        where allVars = getVariables str
              uniqueVars = nub allVars
              finalVars = [[(var !! (length var - i)) |i <- [1..(length var)]] | var <- uniqueVars, var /= []]
              opList = [((mkRegex (finalVars !! i) :: Regex), [intToDigit i]) | i <- [0..(length finalVars)-1]]

getVariables :: String -> [String]
getVariables str = foldl (helper) [[]] str
        where helper [[]] x = if x `elem` "/!&>=#()" then [[]] else [[x]]
              helper (full@(first:rest)) x = if x `elem` "/!&>=#()" then []:full else (x:first):rest
