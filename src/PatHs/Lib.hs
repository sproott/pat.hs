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
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as TextIO
import           PatHs.Config
import           PatHs.Lib.Command
import           PatHs.Types
import           System.IO.Error            (catchIOError)

loadMarks :: AppM Marks
loadMarks = do
  config <- loadConfig
  except $ Map.fromList <$> convertKeys validateKey config

configPath :: HomeDir -> String
configPath homeDir = Text.unpack (unHomeDir homeDir) <> "/.pat-hs"

loadConfig :: AppM Config
loadConfig = do
  homeDir <- liftIO getHomeDirectory'
  !contents <- liftIO $ TextIO.readFile (configPath homeDir) `catchIOError` const (pure "")
  except $ parseConfig homeDir contents

parseConfig :: HomeDir -> Text -> Either Error Config
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
  homeDir <- getHomeDirectory'
  putStrLn $ Text.unpack $ unResolvedValue $ resolveToHomeDir homeDir $ unValue value
consumeResult _ (RTGo value) = do
  homeDir <- getHomeDirectory'
  TextIO.putStrLn $ unResolvedValue value
consumeResult _ (RTList marks)           = do
  homeDir <- getHomeDirectory'
  mapM_ TextIO.putStrLn $ showMarks $ resolveMarks homeDir marks

saveMarks :: Marks -> IO ()
saveMarks marks = do
  homeDir <- getHomeDirectory'
  TextIO.writeFile (configPath homeDir) $ marksToConfigString marks

showMarks :: ResolvedMarks -> [Text]
showMarks marks = uncurry printTuple <$> Map.toList marks
    where printTuple validKey resolvedValue = unValidKey validKey <> "    " <> unResolvedValue resolvedValue

resolveMarks :: HomeDir -> Marks -> ResolvedMarks
resolveMarks homeDir = Map.map (resolveToHomeDir homeDir . unValue)
