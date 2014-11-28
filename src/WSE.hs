

module WSE where


data WSE err state out a = 
    WSE ((state, [out]) -> (Either err a, state, [out]))

instance Monad (WSE err state out) where
    return x = WSE (\ (s, o) -> (Right x, s, o))
    WSE p >>= f = WSE (\ (s, o) ->
                           do let (res, s', o') = p (s, o)
                              case res of
                                Right x -> do
                                         let (WSE f') = f x
                                         f' (s', o')
                                Left err -> (Left err, s', o')
                      )

runWSE :: WSE err state out a -> state -> (Either err a, state, [out])
runWSE (WSE f) s = let (res, s', out) = f (s, []) 
                   in (res, s', reverse out)

throwError :: err -> WSE err state out a
throwError msg = WSE (\(s, o) -> (Left msg, s, o))

catchError :: WSE err state out a ->
              (err -> WSE err state out a) ->
              WSE err state out a
catchError (WSE f) handler =
    WSE (\(s, o) -> 
         case f (s, o) of
           (Left msg, s', o') -> do let (WSE f') = handler msg
                                    f' (s', o')
           p@(Right x, s', o') -> p
        )

get :: WSE err state out state
get = WSE (\ (s, o) -> (Right s, s, o))

put :: state -> WSE err state out ()
put s = WSE (\ (_, o) -> (Right (), s, o))

modify :: (state -> state) -> WSE err state out ()
modify f = do s <- get
              put (f s)

tell :: out -> WSE err state out ()
tell o = WSE (\(s, os) -> (Right (), s, o : os))

