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

    read_t <- forkIO $ readChat s
    writeChat s sa

parseArgs :: [String] -> IO SockAddr
parseArgs (h:p:_) = do
    host <- inet_addr h
    return $ SockAddrInet port host
    where
        port = fromIntegral . read $ p

setupSocket :: IO Socket
setupSocket = do
    s <- socket AF_INET Datagram defaultProtocol
    bindSocket s (SockAddrInet 3000 0)
    return s

readChat :: Socket -> IO ()
readChat s = forever $ do
    (s,l,f) <- recvFrom s 2000
    putStrLn $ (show f) ++ " (" ++ (show l) ++ ") >>> " ++ s

writeChat :: Socket -> SockAddr -> IO ()
writeChat s a = forever $ do
    msg <- getLine
    sendTo s msg a
    putStrLn $ "Sent to " ++ (show a)
