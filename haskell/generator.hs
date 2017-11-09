import Text.Regex
import Text.Regex.Base
import Data.Char

main = do
        input <- getLine
        putStrLn (translate input)

translate :: String -> String
translate [] = []
translate str = do
        let spacedBracketsStr = spaceBrackets str
        let translOperators = removeMultiChar spacedBracketsStr
        filter (/=' ') translOperators

spaceBrackets :: String -> String
spaceBrackets str = subRegex (mkRegexWithOpts "\\)" False True) openBrackets " ) "
        where openBrackets = subRegex (mkRegexWithOpts "\\(" False True) str " ( "

removeMultiChar :: String -> String
removeMultiChar str = foldr (\t -> \accStr -> subRegex (fst t) accStr (snd t)) str [(orReg, " / "), (notReg, " ! "), (andReg, " & "), (impReg, " > "), (equReg, " = "), (nEquReg, " # ")]
        where andReg = mkRegexWithOpts "( and |˄)" False True :: Regex
              orReg = mkRegexWithOpts "( or |˅)" False True :: Regex
              impReg = mkRegexWithOpts "( implies |=>|⇒)" False True :: Regex
              equReg = mkRegexWithOpts "( ((is )*equivalent( with)*)|((is )*equal( to)*))" False True :: Regex
              nEquReg = mkRegexWithOpts "( ((is )*not equivalent( with)*)|((is )*not equal( to)*))" False True :: Regex
              notReg = mkRegexWithOpts "(^not |( not )|~)" False True :: Regex
