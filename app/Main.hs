{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}

module Main where

import           Control.Arrow                     (ArrowChoice (left))
import qualified Data.Map.Strict                   as Map
import qualified Data.Text                         as Text
import           Options.Applicative               (execParser)
import           PatHs.Config
import           PatHs.Lib
import           PatHs.Options                     (commandP)
import           PatHs.Types
import           System.Directory                  (setCurrentDirectory)
import           System.Directory.Internal.Prelude (catchIOError)
import           Text.Megaparsec                   (runParser)

main :: IO ()
main = do
  config <- loadConfig
  print config
  (SomeCommand command) <- execParser commandP
  runPatHs marks command

loadConfig :: IO (Either Error Config)
loadConfig = do
  !contents <- (pure <$> readFile "/home/davidh/.pat-hs") `catchIOError` const (pure $ Left ConfigNotExists)
  pure $ contents >>= parseConfig

parseConfig :: String -> Either Error Config
parseConfig contents = left (const InvalidConfig) $ runParser configParser ".pat-hs" $ Text.pack contents

runPatHs :: Marks -> Command c -> IO ()
runPatHs marks command =
  case runCommand command marks of
    (Left err)     -> print err
    (Right result) -> consumeResult command result

consumeResult :: forall (c :: CommandType). Command c -> ReturnType c -> IO ()
consumeResult _ (RTSave marks)           = saveMarks marks
consumeResult _ (RTDelete marks)         = saveMarks marks
consumeResult _ (RTGet (Value valueStr)) = do
  putStrLn valueStr
  setCurrentDirectory valueStr
consumeResult _ (RTList marks)           = mapM_ putStrLn $ showMarks marks

saveMarks :: Marks -> IO ()
saveMarks marks = mapM_ putStrLn $ showMarks marks

showMarks :: Marks -> [String]
showMarks marks = uncurry printTuple <$> Map.toList marks
    where printTuple validKey (Value value) = unValidKey validKey <> "    " <> value
