import System.Environment (getArgs)
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Control.Error (note)
import qualified System.IO.Strict as S (readFile)

main :: IO ()
main = do
    args <- getArgs
    let action = head args
    db <- parseDb
    case action of
        "get"  -> putStrLn $ M.findWithDefault "Clave no encontrada" (args!!1) db
        "add"  -> updateDb (args!!1) (args!!2) db >>= putStrLn
        "show" -> putStrLn $ show' " => " db
        _      -> putStrLn "Comando inválido"

parseDb :: IO (M.Map String String)
parseDb = do
    dbLines <- lines <$> S.readFile "/Users/cristian/estudios/UNLP/06_2018/scripts/cdc/db"
    return $ M.fromList $ fmap (toTuple . splitOn ",") dbLines
    where toTuple [x,y] = (x,y)

updateDb :: String -> String -> M.Map String String -> IO String
updateDb key value db = do
    let updated = M.insert key value db
    writeFile "/Users/cristian/estudios/UNLP/06_2018/scripts/cdc/db" $ show' "," updated
    return "DB actualizada"

show' :: String -> M.Map String String -> String
show' sep = M.foldlWithKey add ""
    where add result k v = result ++ k ++ sep ++ v ++ "\n"