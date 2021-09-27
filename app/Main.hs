{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}

module Main where

import           Control.Arrow                     (ArrowChoice (left))
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Except        (ExceptT (ExceptT), except,
                                                    runExceptT)
import qualified Data.Map.Strict                   as Map
import qualified Data.Text                         as Text
import           Options.Applicative               (execParser)
import           PatHs.Config
import           PatHs.Lib
import           PatHs.Options                     (commandP)
import           PatHs.Types
import           System.Directory                  (setCurrentDirectory)
import           System.Directory.Internal.Prelude (catchIOError)
import           System.Exit                       (exitFailure)
import           Text.Megaparsec                   (runParser)

type AppM a = ExceptT Error IO a

main :: IO ()
main = do
  result <- runApp app
  case result of
    Left err -> do
      print err
      exitFailure
    Right _ -> pure ()

runApp :: AppM a -> IO (Either Error a)
runApp = runExceptT

app :: AppM ()
app = do
  config <- loadConfig
  liftIO $ print config
  (SomeCommand command) <- liftIO $ execParser commandP
  runPatHs marks command

loadConfig :: AppM Config
loadConfig = do
  !contents <- ExceptT $ (Right <$> readFile "/home/davidh/.pat-hs") `catchIOError` const (pure $ Left ConfigNotExists)
  parseConfig contents

parseConfig :: String -> AppM Config
parseConfig contents = except $ left (const InvalidConfig) $ runParser configParser ".pat-hs" $ Text.pack contents

runPatHs :: Marks -> Command c -> AppM ()
runPatHs marks command = do
  result <- except $ runCommand command marks
  liftIO $ consumeResult command result

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
