{-
  Round-Trip Latency Benchmark
  Course: 7,5 ECTS project
  Date: 23/3/12
  
  Brian Avlund
-}

{-# LANGUAGE DeriveDataTypeable,TemplateHaskell, TypeSynonymInstances #-}
module RTLB where
	
import Remote
import Remote.Call (mkClosure)
import Data.Binary (Binary,get,put,encode,decode)

{- ------------------ Flist ------------------ -}
type FList = Maybe Int -> [String]

instance Binary FList where 
                put = genericPut
                get = genericGet


empty :: FList
empty = \ _ -> []

add :: String -> FList -> FList
add x fl = \ pre ->
  case pre of
    Nothing        -> x : fl Nothing
    Just n | n > 0 -> x : (fl $ Just $ n-1)
    _              -> []
        
fromList :: [String] -> FList
fromList xs = foldr add empty xs

toList :: FList -> [String]
toList fl = fl Nothing

ftake :: Int -> FList -> [String]
ftake n fl = fl $ Just n

{- ------------------ Round trip latency benchmark ------------------ -}
-- NOTE: it seems that receiveTimeout is faulty and do not reset the timer in a new iterationc.
-- next: id of the next process in the loop
-- tag: temperary identifier
receive_send :: ProcessId -> String -> ProcessM ()
receive_send next tag = do {res <- receiveTimeout 200000 [match (\x -> return (x::FList))];
                        case res of
     					   	 Nothing  -> do {selfP <- getSelfPid;
							     			 say ((show selfP) ++ " DIED");
							                 return ()}
     					   	 Just ans -> do send next (add tag ans)
                                                                receive_send next tag}				  


$( remotable ['receive_send] )
		
		
spawnN :: [NodeId] -> Int -> ProcessId -> ProcessM ProcessId			  
spawnN nodes 0 p = return p
spawnN nodes n p0 = do {p1 <- spawnLocal (receive_send p0 "this");
                        pn <- spawnOnNodes nodes p1;
						spawnN nodes (n-1) pn}
			
				
spawnOnNodes :: [NodeId] -> ProcessId -> ProcessM ProcessId
spawnOnNodes [] p = return p
spawnOnNodes (n:nodes) p0 = do {p1 <- spawn n (receive_send__closure p0 "other");
                                spawnOnNodes nodes p1}


initialProcess "MASTER" = do {selfP <- getSelfPid;
							  peers <- getPeers;
							  let {workers = findPeerByRole peers "WORKER";
							       flist = empty};
							  pn <- spawnN workers 5 selfP;
							  n <- receiveLoop 1 flist pn;
							  say ("Finished with n = " ++ (show n))}
						  where
							  receiveLoop 0 f _ = return f
							  receiveLoop i f pid = do {send pid ('add "master" f);
							   					        res <- receiveWait [match (\x -> return (x::Closure String))];
														do {flst <- invokeClosure res;
															case flst of
															    Nothing -> return ()
															    Just fl -> do {receiveLoop (i-1) fl pid}}}

-- initialProcess "WORKER" = receiveTimeout 200000  []							
initialProcess "WORKER" = receiveWait []							

initialProcess _ = say " MASTER or WORKER."

main = remoteInit (Just "config") [RTLB.__remoteCallMetaData] initialProcess 
