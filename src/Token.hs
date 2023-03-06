module Token where

type Token      = String
type TokenType  = String
type TokenID    = String
type LineNumber = Int
data TokenInfo  = TokenInfo {
        getToken      :: Token,
        getTokenType  :: TokenType,
        getTokenID    :: TokenID,
        getLineNumber :: LineNumber 
    } deriving (Show)

tokenToLine :: TokenInfo -> String
tokenToLine tokenInfo = getToken tokenInfo ++ "\t" ++ getTokenType tokenInfo ++ "\t" ++ getTokenID tokenInfo ++ "\t" ++ show (getLineNumber tokenInfo) ++ "\n"

lineToToken :: String -> TokenInfo
lineToToken = (\[token, tokenType, tokenID, lineNumber] -> TokenInfo token tokenType tokenID (read lineNumber)) . splitOnTab
        
splitOnTab :: String -> [String]
splitOnTab string = firstWord : case rest of
    [] -> []
    _  -> splitOnTab rest
    where
        (firstWord, rest) = clipFirstOnTab string
        clipFirstOnTab :: String -> (String, String)
        clipFirstOnTab (h:t) 
            | h == '\t' = ([], t)
            | otherwise = (h:s, s')
            where
                (s, s') = clipFirstOnTab t
        clipFirstOnTab [] = ([], [])