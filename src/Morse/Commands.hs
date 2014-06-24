module Morse.Commands
  ( commands
  , run
  ) where

import Morse.Commands.Server (runServer)
import Morse.Engine

import Options.Applicative


data Command 
  = Server Int
  | Encode String
  | Decode String
  | Code String

morseInputArg :: Parser String
morseInputArg = argument str
              ( metavar "INPUT"
             <> help "convert INPUT" )

serverOpts :: Parser Command
serverOpts = Server 
  <$> argument auto
      ( metavar "PORT"
     <> help "listen on PORT" )

encodeOpts :: Parser Command
encodeOpts = Encode <$> morseInputArg

decodeOpts :: Parser Command
decodeOpts = Decode <$> morseInputArg

codeOpts :: Parser Command
codeOpts = Code <$> morseInputArg

commands :: Parser Command
commands = subparser
  ( command "server"
    ( info serverOpts
          ( progDesc "Start the web server"))
 <> command "encode"
    ( info encodeOpts
          ( progDesc "Encode as morse code" ))
 <> command "decode"
    ( info decodeOpts
          ( progDesc "Decode from morse code" ))
 <> command "code"
    ( info codeOpts
          ( progDesc "Detect type and encode/decode accordingly" ))
  )

runCommand :: Command -> IO ()
runCommand (Server port)   = runServer port
runCommand (Encode input)   = putStrLn (toMorse input)
runCommand (Decode input) = putStrLn (fromMorse input)
runCommand (Code input) = putStrLn (process input)
  where process = case detectProcessType input of
                    PTEncode -> toMorse
                    PTDecode -> fromMorse

run :: IO ()
run = execParser opts >>= runCommand
  where opts = info (helper <*> commands)
          ( fullDesc
         <> header "morse - a program for morsing code." )