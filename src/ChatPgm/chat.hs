{-
 - ChatPgm - Grand Rapids Haskell Users' Group
 -}

module Main where

import System.Environment 
import Network.Socket
import Control.Monad
import Control.Concurrent

main :: IO ()
main = withSocketsDo $ do
    putStrLn "Chat Client 0.0.1"
    args <- getArgs
    sa <- parseArgs args
    s <- setupSocket

    adrs <- newMVar sa

    read_t <- forkIO $ readChat s adrs
    writeChat s adrs

parseArgs :: [String] -> IO [SockAddr]
parseArgs (h:p:_) = do
    host <- inet_addr h
    return $ [SockAddrInet port host]
    where
        port = fromIntegral . read $ p

setupSocket :: IO Socket
setupSocket = do
    s <- socket AF_INET Datagram defaultProtocol
    bindSocket s (SockAddrInet 3000 0)
    return s

readChat :: Socket -> MVar [SockAddr] -> IO ()
readChat s m = forever $ do
    (s,l,f) <- recvFrom s 2000
    addAddr f
    putStrLn $ (show f) ++ " (" ++ (show l) ++ ") >>> " ++ s
    where
        addAddr a = do
            adrs <- takeMVar m
            case elem a adrs of
                True  -> return ()
                False -> putMVar m (a:adrs)

writeChat :: Socket -> MVar [SockAddr] -> IO ()
writeChat s m = forever $ do
    msg <- getLine
    adrs <- readMVar m
    mapM (snd msg) adrs
    where
        snd msg a = do
            sendTo s msg a
            putStrLn $ "Sent to " ++ (show a)
