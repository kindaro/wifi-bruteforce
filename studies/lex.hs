
import Text.Read.Lex as Lex
import Text.ParserCombinators.ReadP

main = do
    readme <- getLine
    let lex_prelude = fst . head . Prelude.lex $ readme
    let lex_lex = (\(String x) -> x) . fst . head . (readP_to_S Lex.lex) $ readme
    putStrLn $ "Got: " ++ readme
    putStrLn $ "Prelude: " ++ lex_prelude
    -- putStrLn $ "Lex: " ++ lex_lex -- Non-exhaustive patterns.

