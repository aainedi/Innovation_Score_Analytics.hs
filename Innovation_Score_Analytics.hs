module Main where


-- Define data type for:
-- 1. Project ID
-- 2. Category
-- 3. List of Scores given by judges
type ProjectData = (String, String, [Int])


-- PROJECT LIST
projects :: [ProjectData]
projects =
    [ ("A001", "AI", [92, 94, 90, 93])
    , ("A002", "AI", [87, 88, 85, 89])
    , ("A003", "AI", [78, 80, 76, 79])

    , ("C001", "CyberSecurity", [95, 96, 94, 97])
    , ("C002", "CyberSecurity", [89, 91, 88, 90])
    , ("C003", "CyberSecurity", [83, 85, 81, 84])

    , ("I001", "IoT", [91, 93, 90, 92])
    , ("I002", "IoT", [86, 88, 84, 87])
    , ("I003", "IoT", [82, 84, 81, 83, 80])

    , ("W001", "Web Development", [81, 85, 79, 83])
    , ("W002", "Web Development", [87, 84, 89, 86])
    , ("W003", "Web Development", [75, 78, 73, 80])
    ]


-- CALCULATE TOTAL SCORE USING RECURSION
totalScore :: [Int] -> Int
totalScore [] =
    0

totalScore (score:scores) =
    score + totalScore scores


-- CALCULATE AVERAGE SCORE
averageScore :: [Int] -> Double
averageScore scores =
    fromIntegral (totalScore scores) / fromIntegral (length scores)


-- CALCULATE PROJECT AVERAGE
projectAverage :: ProjectData -> Double
projectAverage (_, _, scores) =
    averageScore scores


-- CALCULATE AVERAGE FOR ALL PROJECTS USING MAP
projectAverages :: [ProjectData] -> [(String, Double)]
projectAverages projectList =
    map (\(projectId, _, scores) ->
        (projectId, averageScore scores)) projectList


-- FILTER PROJECTS WITH AVERAGE SCORE >= THRESHOLD
filterTopProjects :: Double -> [ProjectData] -> [ProjectData]
filterTopProjects threshold projectList =
    filter (\(_, _, scores) -> averageScore scores >= threshold) projectList


-- FIND THE PROJECT WITH THE HIGHEST AVERAGE SCORE
highestProject :: [ProjectData] -> ProjectData
highestProject [] =
    error "Empty project list"

highestProject [project] =
    project

highestProject (project:xs)
    | projectAverage project >= projectAverage (highestProject xs) =
        project
    | otherwise =
        highestProject xs


-- MAIN PROGRAM
main :: IO()
main = do

    putStrLn "--- Innovation Project Average Scores ---"

    mapM_ print (projectAverages projects)


    putStrLn "\n--- Projects qualify for Final Pitching Round ---"

    mapM_ print
        [ (projectId, category, averageScore scores)
        | (projectId, category, scores) <- filterTopProjects 80 projects
        ]


    putStrLn "\n\n======================="
    putStrLn "Overall Winning Project"
    putStrLn "======================="

    let (winningId, winningCategory, winningScores) =
            highestProject projects

    putStrLn $ "Project ID: " ++ winningId
    putStrLn $ "Category: " ++ winningCategory
    putStrLn $ "Average Score: " ++ show (averageScore winningScores)
