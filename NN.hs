import System.Random

data Perceptron =   Perceptron  { weights :: [Double], rate :: Double } deriving (Show)

randomsN :: Int -> IO [Double]
randomsN n = ((return . randoms =<< newStdGen) :: IO [Double]) >>= return . take n

randomRsN :: (Double, Double) -> Int -> IO [Double]
randomRsN (a,b) n = ((return . randomRs (a,b) =<< newStdGen) :: IO [Double]) >>= return . take n

createPerceptron :: [Double] -> Perceptron
createPerceptron weights = Perceptron weights 0.01

feedForward :: Perceptron -> [Double] -> Double
feedForward p inputs = activate s
    where s = sum $ zipWith (*) inputs (weights p)

train :: Perceptron -> [Double] -> Double -> Perceptron
train p inputs desired = createPerceptron newWeights
    where   
        newWeights = zipWith (+) (map (\x -> (rate p)*err*x) (inputs)) (weights p)
        err = desired - guess
        guess = feedForward p inputs

startTrain :: Perceptron -> [([Double], Double)] -> Perceptron
startTrain p trainSet = foldr (\ts perc -> train perc (fst ts) (snd ts)) p trainSet

activate :: Double -> Double
activate s
    | s > 0     =   1
    | otherwise =  -1

f :: Double -> Double
f x = 2 * x + 1

main = do
    rand <- randomRsN (-1, 1) 3
    let p = createPerceptron rand

    let xmin = -400
        ymin = -100
        xmax = 400
        ymax = 100
        nTrain = 500
        nTrainHalf = truncate (0.5 * fromIntegral nTrain)
        nTrainQuater = truncate (0.5 * fromIntegral nTrainHalf)

    putStrLn "Classification problem: separate points (x,y) with the line 2 * x + 1" 
    putStrLn $ "in the rectangle (-400, -100), (400, 100) using " ++ show nTrainHalf ++ " training examples."
    putStrLn " "

    xs <- randomRsN (xmin,xmax) nTrain
    ys <- randomRsN (ymin,ymax) nTrain
    
    let trainSet = zipWith (\x y -> ([x, y, 1], (if y < f x then (-1) else 1))) xs ys
        trainPerc = startTrain p $ take nTrainHalf trainSet
        nCorrectTrain = length $ 
            filter (== True) $ map (\(input, ans) -> (feedForward trainPerc input) == ans) $ 
            take nTrainHalf trainSet

    putStrLn $ "Correct answers on the training set:\t" ++ 
        show ((fromIntegral nCorrectTrain / fromIntegral nTrainHalf) * 100) ++ "%" 

    let nCorrectTest = length $ 
            filter (== True) $ map (\(input, ans) -> (feedForward trainPerc input) == ans) $ 
            take nTrainQuater $ drop nTrainHalf trainSet

    putStrLn $ "Correct answers on the test set:\t" ++ 
        show ((fromIntegral nCorrectTest / fromIntegral nTrainQuater) * 100) ++ "%" 

    let nCorrect = length $ 
            filter (== True) $ map (\(input, ans) -> (feedForward trainPerc input) == ans) $ 
            take nTrainQuater $ drop (nTrainHalf + nTrainQuater) trainSet

    putStrLn $ "Correct answers on an arbitrary set:\t" ++
        show ((fromIntegral nCorrect / fromIntegral nTrainQuater) * 100) ++ "%" 