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

merge :: FList -> FList -> FList
merge xs ys = foldr add ys $ toList xs

ftake :: Int -> FList -> [String]
ftake n fl = fl $ Just n

{- ------------------ Simple list ------------------ -}
type SList = [String]

emptyS :: SList
emptyS = []

addS :: String -> SList -> SList
addS x l = x : l

fromListS :: [String] -> SList
fromListS xs = xs -- foldr addS empty xs

toListS :: [String] -> [String]
toListS l = l

mergeS :: SList -> SList -> SList
mergeS xs ys = foldr addS ys $ toListS xs

stake :: Int -> SList -> [String]
stake n fl = take n fl


{- ------------------ Round trip latency benchmark ------------------ -}
-- NOTE: it seems that receiveTimeout is faulty and do not reset the timer in a new iteration.
-- tag: temporary identifier
receive_sendR :: ProcessId -> String -> ProcessM ()
receive_sendR next tag = do {res <- receiveTimeout 200000 [match (\x -> return (x::SList))];
                        	case res of
     							Nothing  -> do {selfP <- getSelfPid;
												say ((show selfP) ++ " DIED");
												return ()}
     							Just fl -> do {	send next (addS tag fl);
					    						receive_sendR next tag}}				  

receive_sendT :: (ProcessId, ProcessId) -> String -> ProcessM ()
receive_sendT (lc, rc) tag = do {res <- receiveTimeout 200000 [match (\x -> return (x::SList))];
								case res of
     						  	 	Nothing  -> do { selfP <- getSelfPid;
								 				 	 say ((show selfP) ++ " DIED");
												 	 return ()}
     							 	Just fl -> do { let {nlist = addS tag fl};
     							 				 	send lc nlist;
	     							 				send rc nlist;
						    						receive_sendT (lc, rc) tag}}

$( remotable ['receive_sendR, 'receive_sendT] )
		
		
spawnR :: [NodeId] -> Int -> ProcessId -> ProcessM ProcessId			  
spawnR nodes 0 p = return p
spawnR nodes n p0 = do {p1 <- spawnLocal (receive_sendR p0 "this");
                        pn <- spawnOnNodes nodes p1;
						spawnR nodes (n-1) pn}
			
				
spawnOnNodes :: [NodeId] -> ProcessId -> ProcessM ProcessId
spawnOnNodes [] p = return p
spawnOnNodes (n:nodes) p0 = do {p1 <- spawn n (receive_sendR__closure p0 "other");
                                spawnOnNodes nodes p1}


initialProcess "MASTER" = do {selfP <- getSelfPid;
							  peers <- getPeers;
							  say ("workers = " ++ (show $ findPeerByRole peers "MASTER"));
							  let {workers = findPeerByRole peers "WORKER";
							       flist = emptyS};
							  pn <- spawnR workers 5 selfP;
							  fl <- receiveLoop 1 flist pn;
							  say ("Finished with n = " ++ (show $ toListS fl))}
							  -- say ("Finished with n = " ++ (show $ fl))}
						  where
							  receiveLoop 0 f _ = return f
							  receiveLoop i f pid = do {send pid f;
							   					        fl <- receiveWait [match (\x -> return (x::SList))];
							   					        receiveLoop (i-1) fl pid}

-- initialProcess "WORKER" = receiveTimeout 200000  []							
initialProcess "WORKER" = do { selfP <- getSelfPid;
								say ("worker node " ++ (show selfP));
							   peers <- getPeers;
							   say ("workers = " ++ (show $ findPeerByRole peers "WORKER"));
							   say ("peers = " ++ (show $ peers));
							   receiveWait []}

initialProcess _ = say "MASTER or WORKER."

main = remoteInit (Just "config") [RTLB.__remoteCallMetaData] initialProcess 


{- ------------------ Tree structure ------------------ -}
				  

spawnT :: Integer -> NodeId -> ProcessId -> ProcessM (ProcessId, ProcessId)
spawnT 0 nodes self = return (self, self)
spawnT s node self = do {c0 <- spawnT (s-1) node self;
				   	  	 lc <- spawnLocal (receive_sendT c0 "this");
				   	  	 c1 <- spawnT (s-1) node self;
					  	 rc <- spawn node (receive_sendT__closure c1 "other");
				   	  	 return (lc, rc)}

gather_list :: Integer -> SList -> ProcessM SList
gather_list 0 list = return list
gather_list i list = do {fl <- receiveWait [match (\x -> return (x::SList))];
					  	 gather_list (i-1) $ mergeS fl list}

initialProcessT "MASTER" = do { let {layers = 1};
							   	selfP <- getSelfPid;
							   	peers <- getPeers;
							    let {w:ws = findPeerByRole peers "WORKER";
							         flist = addS "master" emptyS};
							    pn <- spawnT layers w selfP;
								fl <- receiveLoop 1 flist pn layers;
							   	say ("Finished with n = " ++ (show $ toListS fl))}
						   where
								receiveLoop 0 f _ _ = return f
							   	receiveLoop i f (lc, rc) layers = do {send lc f;
							   										  send rc f;
							    					         	 	  fl <- gather_list (2^(layers+1)) f;
							    					      		 	  receiveLoop (i-1) fl (lc, rc) layers}

-- initialProcess "WORKER" = receiveTimeout 200000  []							
initialProcessT "WORKER" = do { peers <- getPeers;
							    say ("peers = " ++ (show $ peers));
							    receiveWait []}						

initialProcessT _ = say " MASTER or WORKER."

mainT = remoteInit (Just "config") [RTLB.__remoteCallMetaData] initialProcessT 
