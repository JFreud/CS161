exp1 =
    do
        [(),()]
        x <- [1,2,3]
        pure x

exp1' = [(),()] >>= \x -> [1,2,3]
exp1'' = [(),()] >> [1,2,3]


exp2 =
    do
        x <- [1,2,3]
        [(),()]
        pure x

exp2' = [1,2,3] >>= \x -> [(x), (x)]

doWhile_ :: IO a -> (a -> Bool) -> IO ()
doWhile_ action predic = do
  a <- action   -- action >>= \a ->
  if predic a then
    doWhile_ action predic
  else
    pure ()

-- doWhile :: IO a -> (a -> Bool) -> IO [a]
doWhile :: Monad m => m a -> (a -> Bool) -> m [a]
doWhile action predic = do
  a <- action   -- action >>= \a ->
  if predic a then do
    as <- doWhile action predic
    pure $ a : as
  else
    pure []


getLinesUntilEmpty :: IO [String]
getLinesUntilEmpty =
  doWhile (putStrLn "..." >> getLine) (/= "")


-- getLinesUntilEmpty :: IO ()
-- getLinesUntilEmpty = do
--   putStrLn "Say something" -- putStrLn "asdfa" >>
--   s <- getLine             -- getLine >>= \s ->
--   if s == "" then
--     pure ()
--   else
--     getLinesUntilEmpty
--   case s of
--     "" -> pure ()
--     _  -> getLinesUntilEmpty
--   pure ()                  -- pure ()


main :: IO ()
main = do
  -- getLines
  lines <- getLinesUntilEmpty

  -- reverse each line, reverse lines
  let lines' = reverse $ map reverse lines

  -- print
  putStrLn $ unlines lines'
