{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Debug where

import Types

appendLogToGame :: String -> Game -> Game
appendLogToGame s game = game {logs = logs game ++ [s]}

appendLogsToGame :: [String] -> Game -> Game
appendLogsToGame ss game = game {logs = logs game ++ ss}

appendKeyValueLogToGame :: String -> String -> Game -> Game
appendKeyValueLogToGame k v game = game {logs = logs game ++ [k ++ ": " ++ v]}

appendLog :: String -> [String] -> [String]
appendLog msg l = l ++ [msg]

appendKeyValueLog :: String -> String -> [String] -> [String]
appendKeyValueLog k v l = l ++ [k ++ ": " ++ v]