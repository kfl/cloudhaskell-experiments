{-
  Round-Trip Latency Benchmark
  Course: 7,5 ECTS project
  Date: 23/3/12
  
  Brian Avlund
-}

{-# LANGUAGE TemplateHaskell #-}
module RTLB where
	
import Remote

{- ------------------ Flist ------------------ -}
type FList a = Maybe Int -> [a]

empty :: FList a
empty = \ _ -> []

add :: a -> FList a -> FList a
add x fl = \ pre -> 
  case pre of
    Nothing        -> x : fl Nothing
    Just n | n > 0 -> x : (fl $ Just $ n-1)
    _              -> []
        
fromList :: [a] -> FList a
fromList xs = foldr add empty xs

toList :: FList a -> [a]
toList fl = fl Nothing
        
ftake :: Int -> FList a -> [a]
ftake n fl = fl $ Just n


{- ------------------ Round trip latency benchmark ------------------ -}
-- NOTE: it seems that receiveTimeout is faulty and do not reset the timer in a new iterationc.
-- next: id of the next process in the loop
receive_send :: ProcessId -> ProcessM ()
receive_send next = do {res <- receiveTimeout 200000 [match (\x -> return (x::Int))];
                        case res of
     					   	 Nothing  -> do {selfP <- getSelfPid;
							     			 say ((show selfP) ++ " DIED");
							                 return ()}
     					   	 Just ans -> do {send next (ans + 1);
					                         receive_send next}}
					   
$( remotable ['receive_send] )
		
spawnN :: [NodeId] -> Int -> ProcessId -> ProcessM ProcessId			  
spawnN nodes 0 p = return p
spawnN nodes n p0 = do {p1 <- spawnLocal (receive_send p0);
                        pn <- spawnOnNodes nodes p1;
						spawnN nodes (n-1) pn}
					where
						spawnOnNodes [] p = return p
						spawnOnNodes (n:nodes) p0 = do {p1 <- spawn n (receive_send__closure p0);
                                						spawnOnNodes nodes p1}

initialProcess "MASTER" = do {selfP <- getSelfPid;
							  peers <- getPeers;
							  let {workers = findPeerByRole peers "WORKER"};
							  pn <- spawnN workers 10 selfP;
							  n <- receiveLoop 1000 0 pn;
							  say ("Finished with n = " ++ (show n))}
						  where
							  receiveLoop 0 n _ = return n
							  receiveLoop i n pid = do {send pid ((n+1)::Int);
							   					        res <- receiveWait [match (\x -> return (x::Int))];
													    receiveLoop (i-1) (res) pid}

initialProcess "WORKER" = receiveWait []							

initialProcess _ = say " MASTER or WORKER."

main = remoteInit (Just "config") [RTLB.__remoteCallMetaData] initialProcess 
