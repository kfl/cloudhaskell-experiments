{-
  Round-Trip Latency Benchmark
  Course: 7,5 ECTS project
  Date: 23/3/12
  
  Brian Avlund
-}

{-# LANGUAGE TemplateHaskell #-}
module RTLB where
	
import Remote

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

initialProcess "MASTER" = do {self <- getSelfNode;
                              selfP <- getSelfPid;
							  pn <- spawnN 10 selfP; 
							  n <- receiveLoop 1000 0 pn;
							  say ("Finished with n = " ++ (show n))}
						  where
							  spawnN 0 p = return p
							  spawnN n p0 = do {p <- spawnLocal (receive_send p0);
							                    spawnN (n-1) p}
							  receiveLoop 0 n _ = return n
							  receiveLoop i n pid = do {send pid ((n+1)::Int);
							   					        res <- receiveWait [match (\x -> return (x::Int))];
													    --say (("ITERATION ") ++ (show i));
														receiveLoop (i-1) (res) pid}

initialProcess _ = say "You need to start this program as either a MASTER or a WORKER. Set the appropiate value of cfgRole on the command line or in the config file."

main = remoteInit (Just "config") [RTLB.__remoteCallMetaData] initialProcess 
