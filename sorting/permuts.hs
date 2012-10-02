import Data.List;

dfs (lefts, takens) = do
  n <- lefts
  return (delete n lefts, n:takens)

mapMFns fns val = 
  foldl (\acc f -> acc >>= f) val fns

permuts x = 
  let fns = take x $ repeat (dfs)
  in mapMFns fns $ return ([1..x], [])

takens (_, takens) = takens

printDataSet xs = do
  putStrLn dataSetParsed
  return ()
    where
      len = length xs
      dataSet = map show $ len:xs
      dataSetParsed = intercalate " " $ dataSet

main = do 
  line <- getLine
  let x = (read line :: Int)
  let results = permuts x
  putStrLn $ show $ length results
  mapM_ (printDataSet.takens) results
  return 0
