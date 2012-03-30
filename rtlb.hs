{-
  Round-Trip Latency Benchmark
  Course: 7,5 ECTS project
  Date: 30/3/12
  
  Brian Avlund
-}

{-# LANGUAGE DeriveDataTypeable,TemplateHaskell, TypeSynonymInstances #-}
module Main where

import Structs

import Remote
import Remote.Call (mkClosure)
--import Data.Binary (Binary,get,put,encode,decode)

{- ------------------ Remote methods ------------------ -}
receive_sendR :: ProcessId -> String -> ProcessM ()
receive_sendR next tag = do {res <- receiveWait [match (\x -> return (x::SInt))];
                        	 send next (addSi 1 res)}				  

$( remotable ['receive_sendR] )


{- ------------------ Round trip latency benchmark ------------------ -}
spawnR :: [NodeId] -> Int -> ProcessId -> ProcessM ProcessId			  
spawnR nodes 0 p = return p
spawnR nodes n p0 = do {p1 <- spawnLocal (receive_sendR p0 "this");
                        pn <- spawnOnNodes nodes p1;
						spawnR nodes (n-1) pn}


spawnOnNodes :: [NodeId] -> ProcessId -> ProcessM ProcessId
spawnOnNodes [] p = return p
spawnOnNodes (n:nodes) p0 = spawn n $ receive_sendR__closure p0 "other"


initialProcess :: String -> ProcessM ()
initialProcess "MASTER" = do {selfP <- getSelfPid;
							  peers <- getPeers;
							  let {workers = findPeerByRole peers "WORKER";
							       flist = emptySi};
							  pn <- spawnR workers 100000 selfP;
							  fl <- receiveLoop 1 flist pn;
							  say ("Finished with n = " ++ (show $ toListSi fl))}
						  where
							  receiveLoop 0 f _ = return f
							  receiveLoop i f pid = do {send pid f;
							   					        fl <- receiveWait [match (\x -> return (x::SInt))];
							   					        receiveLoop (i-1) fl pid}

initialProcess "WORKER" = do { peers <- getPeers;
							   say ("workers = " ++ (show $ findPeerByRole peers "WORKER"));
							   receiveWait []}

initialProcess _ = say "MASTER or WORKER."

{- ------------------ Initial ------------------ -}
main = remoteInit (Just "config") [Main.__remoteCallMetaData] initialProcess