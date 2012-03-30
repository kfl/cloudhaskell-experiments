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
receive_sendT :: (ProcessId, ProcessId) -> String -> ProcessM ()
receive_sendT (lc, rc) tag = do {res <- receiveWait [match (\x -> return (x::SList))];
								 let {nlist = addS tag res};
				 				 send lc nlist;
					 			 send rc nlist}

$( remotable ['receive_sendT] )

{- ------------------ Tree structure ------------------ -}
spawnT :: Integer -> [NodeId] -> ProcessId -> ProcessM (ProcessId, ProcessId)
spawnT 0 _ self = return (self, self)
spawnT s [] self = do {c0 <- spawnT (s-1) [] self;
				   	   lc <- spawnLocal (receive_sendT c0 "this");
				   	   c1 <- spawnT (s-1) [] self;
					   rc <- spawnLocal (receive_sendT c1 "other");
				   	   return (lc, rc)}
spawnT s nodes self = do {c0 <- spawnT (s-1) nodes self;
				   	  	 lc <- spawnLocal (receive_sendT c0 "this");
				   	  	 c1 <- spawnT (s-1) nodes self;
					  	 rc <- spawn (head nodes) (receive_sendT__closure c1 "other");
				   	  	 return (lc, rc)}


gather_list :: Integer -> SList -> ProcessM SList
gather_list 0 list = return list
gather_list i list = do {fl <- receiveWait [match (\x -> return (x::SList))];
					  	 gather_list (i-1) $ mergeS fl list}


initialProcess :: String -> ProcessM ()
initialProcess "MASTER" = do { let {layers = 10};
							   	selfP <- getSelfPid;
							   	peers <- getPeers;
							    let {nodes = findPeerByRole peers "WORKER";
							         flist = addS "master" emptyS};
							    pn <- spawnT layers nodes selfP;
								fl <- receiveLoop 1 flist pn layers;
							   	say ("Finished with n = " ++ (show $ length $ toListS fl))}
						   where
								receiveLoop 0 f _ _ = return f
							   	receiveLoop i f (lc, rc) layers = do {send lc f;
							   										  send rc f;
							    					         	 	  fl <- gather_list (2^(layers+1)) f;
							    					      		 	  receiveLoop (i-1) fl (lc, rc) layers}

initialProcess "WORKER" = do { peers <- getPeers;
							    say ("peers = " ++ (show $ peers));
							    receiveWait []}						

initialProcess _ = say " MASTER or WORKER."

{- ------------------ Initial ------------------ -}
main = remoteInit (Just "config") [Main.__remoteCallMetaData] initialProcess