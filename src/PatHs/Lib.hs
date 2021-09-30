{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}

module PatHs.Lib where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (except)
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
  !contents <- liftIO $ readFile (configPath homeDir) `catchIOError` const (pure "")
  except $ parseConfig homeDir contents

parseConfig :: FilePath -> String -> Either Error Config
parseConfig homeDir = parse InvalidConfig (configParser homeDir)

convertKeys :: (a -> Either e a') -> [(a, b)] -> Either e [(a', b)]
convertKeys f = traverse $ bitraverse f pure

runPatHs :: Marks -> Command c -> AppM ()
runPatHs marks command = do
  result <- except $ runCommand command marks
  liftIO $ consumeResult command result

consumeResult :: forall (c :: CommandType). Command c -> ReturnType c -> IO ()
consumeResult _ (RTSave marks)           = saveMarks marks
consumeResult _ (RTDelete marks)         = saveMarks marks
consumeResult _ (RTGet value) = do
  homeDir <- getHomeDirectory
  putStrLn $ unResolvedValue $ resolveToHomeDir homeDir $ unValue value
consumeResult _ (RTList marks)           = do
  homeDir <- getHomeDirectory
  mapM_ putStrLn $ showMarks $ resolveMarks homeDir marks

saveMarks :: Marks -> IO ()
saveMarks marks = do
  homeDir <- getHomeDirectory
  writeFile (configPath homeDir) $ marksToConfigString marks

showMarks :: ResolvedMarks -> [String]
showMarks marks = uncurry printTuple <$> Map.toList marks
    where printTuple validKey resolvedValue = unValidKey validKey <> "    " <> unResolvedValue resolvedValue

resolveMarks :: FilePath -> Marks -> ResolvedMarks
resolveMarks homeDir = Map.map (resolveToHomeDir homeDir . unValue)
