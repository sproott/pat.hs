{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}

module PatHs.Lib where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (ExceptT (ExceptT), except)
import           Data.Bitraversable         (bitraverse)
import qualified Data.Map.Strict            as Map
import           PatHs.Config
import           PatHs.Lib.Command
import           PatHs.Types
import           System.Directory           (getHomeDirectory)
import           System.IO.Error            (catchIOError)

loadMarks :: AppM Marks
loadMarks = do
  config <- loadConfig
  except $ Map.fromList <$> convertKeys validateKey config

configPath :: FilePath -> FilePath
configPath homeDir = homeDir <> "/.pat-hs"

loadConfig :: AppM Config
loadConfig = do
  homeDir <- liftIO getHomeDirectory
  !contents <- ExceptT $ (Right <$> readFile (configPath homeDir)) `catchIOError` const (pure $ Left ConfigNotExists)
  except $ parseConfig contents

parseConfig :: String -> Either Error Config
parseConfig = parse InvalidConfig configParser

convertKeys :: (a -> Either e a') -> [(a, b)] -> Either e [(a', b)]
convertKeys f = traverse $ bitraverse f pure

runPatHs :: Marks -> Command c -> AppM ()
runPatHs marks command = do
  result <- except $ runCommand command marks
  liftIO $ consumeResult command result

consumeResult :: forall (c :: CommandType). Command c -> ReturnType c -> IO ()
consumeResult _ (RTSave marks)           = saveMarks marks
consumeResult _ (RTDelete marks)         = saveMarks marks
consumeResult _ (RTGet (Value valueStr)) = do
  putStrLn valueStr
consumeResult _ (RTList marks)           = mapM_ putStrLn $ showMarks marks

saveMarks :: Marks -> IO ()
saveMarks marks = do
  homeDir <- getHomeDirectory
  writeFile (configPath homeDir) $ marksToConfigString marks

showMarks :: Marks -> [String]
showMarks marks = uncurry printTuple <$> Map.toList marks
    where printTuple validKey (Value value) = unValidKey validKey <> "    " <> value
