module GradesOrganiser where 
    
import System.IO
import Data.Char
import Control.Exception (evaluate)
import Control.DeepSeq (force)
import Text.Printf (printf)
import System.Directory (doesFileExist)
import System.IO (writeFile)

commandsLoop :: IO ()
commandsLoop = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin LineBuffering

    putStr "cmd>"
    input <- getLine
    case  dropWhile (== ' ') input  of
        "help"        -> printCommands >> commandsLoop
        "add grade"   -> addGrade >> commandsLoop
        "remove grade"-> removeGrade >> commandsLoop
        "show grades" -> printFile >> commandsLoop
        "average"     -> average >> commandsLoop
        "surprise"    -> gojo >> commandsLoop
        "reset"       -> reset >> commandsLoop
        ""            -> commandsLoop
        "exit"        -> putStrLn "-------------------------------------------"
        _ -> putStrLn "This command doesn't exist" >> commandsLoop

printCommands :: IO ()
printCommands = putStrLn $ "\nACTIONS:"
                        ++ "\n---------"
                        ++ "\naverage"
                        ++ "\nshow grades"
                        ++ "\nadd grade"
                        ++ "\nremove grade"
                        ++ "\nreset"
                        ++ "\nsurprise"
                        ++ "\nexit\n"

printFile :: IO ()
printFile = do
    withFile "grades.txt" ReadMode (\file ->
        do
            text <- hGetContents file
            putStrLn text
        )

addGrade :: IO ()
addGrade = withFile "grades.txt" AppendMode (\file ->
    do
        putStrLn "What is the name of the module?"
        name <- getLine

        putStrLn "How many ECTS?"
        ects <- getLine

        putStrLn "Which grade did you get?"
        grade <- getLine

        hPutStrLn file (name ++ " " ++ "(" ++ ects ++ ")" ++ ": " ++ grade)
    )

removeGrade :: IO()
removeGrade = do
        putStrLn "Which module do you want to remove? (by name)"
        grade <- getLine
        rawText <- readFile "grades.txt"
        _ <- evaluate (force rawText)
        let modules = lines rawText 
            filteredModules = cut takeName grade modules
        file <- openFile "grades.txt" WriteMode  
        mapM_ (hPutStrLn file) filteredModules
        hClose file
     where
        cut :: (String -> String) -> String -> [String] -> [String]
        cut f searched modules = [module' | module' <- modules, fix (f module') /= searched]
        
        takeName :: String -> String
        takeName [] = ""
        takeName module' = helper "" module'
        
        helper :: String -> String -> String
        helper acc (x:xs) | not $ isDigit x = helper (acc ++ [x]) xs
                  | otherwise = acc
        
        fix :: String -> String
        fix = reverse . dropWhile (== ' ') . reverse . filter (/= '(')
       

average :: IO ()
average = withFile "grades.txt" ReadMode ( \file -> do
        rawText <- hGetContents file
        let result = calcAverage rawText
        putStrLn $ "The average of your grades is " ++ show result ++ " ≈ " ++ formatNumber result
       )
    where
        firstDigit :: [String] -> Integer
        firstDigit [] = error "There aro no modules"
        firstDigit (x:xs) | all isDigit x =  read x
                          | head x == '(' = read $ [(head . tail) x]
                          | otherwise = firstDigit xs
        
        dotProduct :: Num a => [a] -> [a] -> [a]
        dotProduct []      []    = []
        dotProduct (x:xs) (y:ys) = (x * y) : dotProduct xs ys
        dotProduct _        _    = error "This code has some problems, contact the developer!"

        calcAverage rawText =
            let modules = lines rawText
                contentsOfModules = map words modules
                listECTS   = map firstDigit contentsOfModules
                listGrades = map (read . last) contentsOfModules
                listPerentageECTS = map ((/ fromIntegral ( sum listECTS)) . fromIntegral) listECTS
                result = sum $ dotProduct listPerentageECTS listGrades 
            in result
        
        formatNumber :: Double -> String
        formatNumber = printf "%.2f"

reset::IO()
reset = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    putStrLn "\n!! Are you sure you want to reset? (All your data will be lost) !! (y/n)"
    answer <- getChar
    if answer == 'y' then do 
        file <- openFile "grades.txt" WriteMode
        hPutStr file ""
        putStrLn "✓ Reset was successful"
        hClose file
        hSetEcho stdin True
        hSetBuffering stdin LineBuffering
    else
        do
        putStrLn "× Process aborted"
        hSetEcho stdin True
        hSetBuffering stdin LineBuffering

ensureGradesFile :: IO ()
ensureGradesFile = do
    exists <- doesFileExist "grades.txt"
    if not exists
        then writeFile "grades.txt" ""  -- create empty file
        else return ()

mainFunction :: IO()
mainFunction = do
    putStrLn "--------------- Welcome to the Grades Organiser ------------------"
    putStrLn ""
    putStrLn "+--------------------------------------------------------------+"
    putStrLn "|   ⚐  Type \"help\" to see a list of available commands         |"
    putStrLn "+--------------------------------------------------------------+"
    ensureGradesFile
    commandsLoop


gojo :: IO ()
gojo = putStrLn "\
\⠀             ⣾⡳⣼⣆⠀⠀⢹⡄⠹⣷⣄⢠⠇⠻⣷⣶⢀⣸⣿⡾⡏⠀⠰⣿⣰⠏⠀⣀⡀⠀⠀⠀⠀⠀⠀⠀\n\
\⠀⠀⠀⠀⠀⠀⠀⠀⠀⣀⡀⣀⣀⣀⡹⣟⡪⢟⣷⠦⠬⣿⣦⣌⡙⠿⡆⠻⡌⠿⣦⣿⣿⣿⣿⣦⣿⡿⠟⠚⠉⠀⠉⠳⣄⡀⠀⠀⠀\n\
\⠀⠀⠀⠀⠀⠀⠀⡀⢀⣼⣟⠛⠛⠙⠛⠉⠻⢶⣮⢿⣯⡙⢶⡌⠲⢤⡑⠀⠈⠛⠟⢿⣿⠛⣿⠋⠀⠀⠀⠀⠀⠀⠀⠀⠀⠙⣆⠀⠀⠀\n\
\⠀⠀⠀⠀⠀⡸⠯⣙⠛⢉⣉⣙⣿⣿⡳⢶⣦⣝⢿⣆⠉⠻⣄⠈⢆⢵⡈⠀⠀⢰⡆⠀⣼⠓⠀⠀⠀   Nah    ⠈⣷⠀⠀\n\
\⠀⠀⠀⠖⠉⠻⣟⡿⣿⣭⢽⣽⣶⣈⢛⣾⣿⣧⠀⠙⠓⠀⠑⢦⡀⠹⣧⢂⠀⣿⡇⢀⣿⠺⠇⠀    I'd⠀     ⣿⠀⠀\n\
\⠀⠀⠀⠀⠐⠈⠉⢛⣿⣿⣶⣤⣈⠉⣰⣗⡈⢛⣇⠀⣵⡀⠀⠘⣿⡄⢻⣤⠀⢻⡇⣼⣧⣿⡄⠀⠀   Win⠀    ⠀⡿⠀⠀\n\
\⠀⠀⠀⠀⠀⣠⣾⣿⢍⡉⠛⠻⣷⡆⠨⣿⣭⣤⣍⠀⢹⣷⡀⠀⠹⣿⡄⠈⠀⢿⠁⣿⣿⠏            ⠀⠀⠀⣇⠀⠀\n\
\⠀⣿⣇⣠⣾⣿⣛⣲⣿⠛⠀⠀⢀⣸⣿⣿⣟⣮⡻⣷⣤⡙⢟⡀⠀⠙⢧⠀⠀⠎⠀⠉⠁⠰⣿⠀⠀         ⠀⢀⡿⠀⠀\n\
\⠀⠈⢻⣿⣿⣽⣿⣿⣿⣴⡏⠚⢛⣈⣍⠛⠛⠿⢦⣌⢙⠻⡆⠁⠀⠀⠀⣴⣦⠀⠀⠀⠐⢳⢻⣦⣀⠀⠀⠀⠀⠀⠀⠀⠀⢀⠮⠀⠀⠀\n\
\⠀⠀⠈⠙⣿⣧⣶⣿⠿⣧⣴⣿⢻⡉⠀⢀⣠⣴⣾⡟⠿⠃⠁⣠⣤⡶⣾⡟⠅⠀⣀⡄⠀⣾⢸⣿⣏⢻⢶⣦⣤⣤⣄⢶⣾⣿⣡⣤⡄⠀\n\
\⠀⠀⣠⣞⣋⣿⣿⣾⣿⡿⡛⣹⡟⣤⢰⡿⠟⠉⣀⣀⣤⣤⡠⠙⢁⣾⡿⠂⠀⣿⠟⣁⠀⣹⠀⣹⣿⡟⣼⣿⣿⣌⣿⣞⣿⣿⠁⠀⠀⠀\n\
\⠀⢠⡿⢛⢟⣿⣿⣿⣿⣿⣿⡟⣼⣿⣟⢓⠛⣿⣏⣿⣵⣗⣵⣴⣿⢟⡵⣣⣼⣿⢟⣵⣶⢻⣶⣿⠀⠀⣈⢻⣿⣿⣿⢿⣾⢿⣧⠀⠀⠀\n\
\⠀⠘⠃⢸⣿⡾⣿⣿⣿⣿⣯⣿⣿⣿⣶⣿⣿⣟⣾⡿⣫⣿⣿⣿⣽⣿⣿⣿⣿⢫⣾⣿⣿⣿⣿⣿⣴⡆⣻⣿⡏⣿⢻⣧⣿⡿⣿⡆⠀⠀\n\
\⠀⠀⠀⠜⣿⣾⢿⣿⣿⣿⣾⣿⣿⣿⣿⣿⣿⣭⣿⣖⣿⢿⣿⡿⣿⣿⣿⡿⢡⢯⣿⣿⣿⣿⣿⣿⣿⣧⡿⣾⣷⣿⣿⢿⣿⡇⠉⠁⠀⠀\n\
\⠀⠀⠀⠀⣿⣥⣾⣿⣿⣿⣿⣿⣿⣿⡇⣭⣿⣿⣿⣿⠃⠞⠟⣸⣿⠏⣸⣧⣀⠿⢿⣿⣿⣟⣿⣿⣿⣿⣽⣿⢿⣿⣿⣿⣿⠁⠀⠀⠀⠀\n\
\⠀⠀⠀⠈⠛⣹⣿⣿⣿⣿⢿⣿⣿⣿⣿⣿⣟⣿⣿⡿⢶⣦⣄⣿⠏⠀⣿⣟⣿⣶⠾⣿⣟⣋⣛⣿⣿⣿⣿⡇⣻⣿⣿⣿⡏⠀⠀⠀⠀⠀\n\
\⠀⠀⠀⠀⠟⠛⠫⣿⣿⣿⣿⣿⡿⣧⠛⣿⠛⣿⣿⣿⣷⡌⠹⡟⠀⠀⠉⡟⠋⢠⣾⣿⣿⣿⡟⣿⣿⣿⣿⢀⣿⣿⣿⣿⣧⠀⠀⠀⠀⠀\n\
\⠀⠀⠀⠀⠀⠀⠘⠋⣾⣷⣿⣿⣧⠙⠀⠙⢣⠝⠛⠋⣽⣷⢦⠇⠀⠀⠘⠁⣤⣾⣿⠝⠛⠉⠘⢻⣿⣿⢿⣼⣷⡟⢻⣷⠉⠀⠀⠀⠀\n\
\⠀⠀⠀⠀⠀⠀⠀⠐⠟⢻⣿⣿⣿⡀⠀⠀⠀⠀⠀⠀⠀⠉⠀⠀⠀⠀⠀⠀⠈⠛⠀⠀⠀⠀⠀⣾⠟⠀⢸⣷⣿⡇⠀⠛⠀⠀⠀⠀⠀\n\
\⠀⠀⠀⠀⠀⠀⠀⠀⠀⠛⠁⠀⢹⣇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⣿⣿⣿⡧⠀⠀⠀⠀⠀⠀⠀⠀\n\
\⠀⠀⠀⠀⠀ ⠀⠀⠀⠀⠀⠀⠈⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣾⢻⡿⠈⠁⠀⠀⠀⠀⠀⠀⠀⠀\n\
\⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢹⣇⠀⠀⠀⠀⠀⠀⠀⠀⠲⣄⠀⡄⠆⠀⠀⠀⠀⠀⠀⠀⠀⣼⡏⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀\n\
\⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⣿⣷⡀⠀⠀⠀⠀⠀⠀⠀⠈⠀⠀⠀⠀⠀⠀⣀⠀⠀⣠⣾⣿⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀\n\
\⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣿⢻⣆⠀⠛⠁⠶⣶⣶⣶⣶⣶⣶⡶⠆⠘⠋⣠⡾⢫⣾⡟⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀\n\
\⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⠛⠀⠙⣷⡀⠀⠀⠙⠛⠛⠛⠛⠋⠁⠀⢀⣴⠋⠀⣾⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀\n\
\⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣤⣿⣰⣦⡀⠸⣿⣦⡀⠀⠀⠀⠀⠀⠀⢀⣴⡟⠁⠀⠐⢻⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀\n\
\⠀⠀⠀⠀⠀⠀⠀⠀⢀⣠⣴⣾⣿⣿⣿⡄⢺⣿⡄⠹⣿⠻⢦⣤⣤⣤⣤⣶⣿⡟⢀⣀⠀⠀⢸⣿⣦⣄⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀\n\
\⠀⠀⠀⠀⢀⣠⣴⣾⣿⣿⣿⣿⣿⣿⣿⣿⣮⣿⣿⡀⠹⡷⣦⣀⡀⡀⢸⣿⠏⢠⣾⣿⠀⠀⣾⣿⣿⣿⣿⣶⣄⣀⠀⠀⠀⠀⠀⠀⠀⠀\n\
\⣀⣤⣴⣶⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣧⠀⠘⣷⣻⡟⠀⡼⠁⣴⣿⣿⣯⣥⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣶⣤⣀⠀⠀⠀⠀\n\
\⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣶⣯⣿⣤⣤⣤⣬⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣶⣤⣄"