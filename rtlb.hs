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
receive_send next = do {res <- receiveTimeout 300000 [match (\x -> return (x::Int))];
                        case res of
     					   	 Nothing  -> do {selfP <- getSelfPid;
							     			 say ((show selfP) ++ " DIED");
							                 return ()}
     					   	 Just ans -> do {send next (ans + 1);
					                         receive_send next}}
					   
$( remotable ['receive_send] )

initialProcess "MASTER" = do {self <- getSelfNode;
                              selfP <- getSelfPid;
							  p1 <- spawnLocal (receive_send selfP);
							  p2 <- spawnLocal (receive_send p1);
							  p3 <- spawnLocal (receive_send p2);
							  p4 <- spawnLocal (receive_send p3);
							  p5 <- spawnLocal (receive_send p4);
							  n <- receiveLoop 50 0 p5;
							  say ("Finished with n = " ++ (show n))}
						  where
							  receiveLoop 0 n _ = return n
							  receiveLoop i n pid = do {send pid ((n+1)::Int);
							   					        res <- receiveWait [match (\x -> return (x::Int))];
													    say (("ITERATION ") ++ (show i));
														receiveLoop (i-1) (res) pid}

initialProcess _ = say "You need to start this program as either a MASTER or a WORKER. Set the appropiate value of cfgRole on the command line or in the config file."

main = remoteInit (Just "config") [RTLB.__remoteCallMetaData] initialProcess 
