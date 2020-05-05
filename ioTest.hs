--testing IO operations for testing 
module Main where
import System.IO --readfile appendfile putstrln
import Data.Char --isdigit
import System.Environment --getargs
import Data.List.Utils --replace

{- main - the guts of the program, runs all of the name changing and 
 -        file creation/editing using the helper functions below
 -
 -      to compile and run entire test suite: 
 -         > ghc ioTest.hs 
 -         > run.sh
 -         > ghci ans.hs
 -         Main> runTestTT allTests
 -  
 -  in order for initial ghc call to compile, ans.hs must already exist with the 
 -  declaration: 'module HWX where'. otherwise it doesn't matter what's already in the file
 -  allTests refers to a TestList created in the ans.hs file after the first ghc call 
 -
 -  run.sh is a bash script that simply finds all .hs files in a given directory and then for each
 - file it compiles it and runs ./ioTest file.
 -
 -     to test just one file:
 -             > ghc ioTest.hs
 -             > ./ioTest "filepath/filename.hs"
 -             > ghci ans.hs
 -             Main> runTestTT tests[STUDENTID]
 -     while this will work, the program does not have a way to handle files that
 -     don't exist and will throw a runtime error if you pass it a filename/filepath that 
 -     doesn't exist, as it was built with the expectation of being run with run.sh, which ensures
 -     the file exists because it gets all the files in the given directory.
 -}
main :: IO () 
main = do
         args <- getArgs
         let file = parse args
         inp <- readFile file
         let id = getStudID inp
             new = findFunc id (lines inp)
             funcs = ["countZeros", "printRes"]
             cases = getCases funcs id
         appendFile "ans.hs" (unlines new)
         appendFile "ans.hs" (" tests" ++id ++ " = TestList [ \n")
         mapM (appendFile "ans.hs") cases
         appendFile "ans.hs" "\t\t] \n"

{- parse - parses the cmd line arguments to get the file to modify
 - @param args expects a singleton list with exactly one file name, otherwise throws an error
 - @returns file name as a string
 -}
parse :: [String] -> String
parse [] = error "provide file to work with"
parse [f] = f
parse _ = error "too many arguments" 

{- getStudID - retrieves the student id, which is expected to be in a comment in the very 
  -             first line of the file (will cause later problems with compiling ans.hs file if there
  -             isn't a comment with at least one number to give it a unique function name)
  - @param file contents, lazily loaded from readFile
  - @returns student id as a string, empty string if not there              
  -}
getStudID :: String -> String
getStudID ('-':file) = getStudID file
getStudID (c:file) | isDigit c = c:getStudID file
                   | otherwise = ""

{- findFunc - looks through the file line by line to find all instances of hard-coded functions and
 -            replaces them with the function name concatened with the student's id to give it a 
 -            unique function name in the new ans.hs file 
 -
 -} 
findFunc :: String -> [String] -> [String]
findFunc _ [] = []
findFunc id (('-':('-':_)):ls) = findFunc id ls
findFunc id ((' ':'m':'o':'d':'u':'l':'e':_):ls) = findFunc id ls
findFunc id (('m':'o':'d':'u':'l':'e':_):ls) = findFunc id ls
findFunc id (l:ls) | (contains "countZeros" l) && (contains "printRes" l) = ((replace "printRes" ("printRes" ++ id) . replace "countZeros" ("countZeros" ++ id)) l):findFunc id ls
                   | contains "countZeros" l =  (replace "countZeros" ("countZeros" ++ id) l):findFunc id ls
                   | contains "printRes" l = (replace "printRes" ("printRes" ++id) l):findFunc id ls
                   | otherwise = l:findFunc id ls

{- correct answers and inputs for each function to be used in the getCases function
 - 
 - to add a function to the test suite, add the corresponding values below, making sure
 - the correct field has a leading and trailing space, and the input has a leading space
 - or else test cases will not be properly formatted and ans.hs won't compile 
 -
 - to run different tests on the existing functions, simply update the correct and input fields,
 - or use (:) to add more cases in the getCases function below
 -} 
correctCZ :: String
correctCZ = " 1 "
inpCZ :: String
inpCZ = " [0,1,1]"
correctPR :: String
correctPR = " \"there are 1 zeros\" "
inpPR :: String
inpPR = " 1"
{- getCases - generates the test cases for each function to be appended to file by main
 -            with appropriate formatting according to HUnit, including no trailing comma on the last test case
 - 
 - proper test case format for reference: TestCase (assertEqual label correctValue (function inputs))
 -   test cases are held in a TestList, format: TestList[ testCase1, testCase2]
 -
 - to add a function to the test suite, add the appropriate bindings below for the singleton and (f:fs) 
 - patterns with the appropriate correct and input fields from above, and also add the function name as
 - a string to the functions list in the main function
 -
 - @param functions, list of functions to create test cases for 
 - @param id, student id to be appended to function names, to test each unique student's functions
 - @returns a list of test cases in string form with proper formatting to be appended by main 
 -
 -} 
getCases :: [String] -> String ->[String]
getCases [] _  = []
getCases [f] id | f == "countZeros" =("\t\tTestCase (assertEqual \"" ++ f ++ "\"" ++ correctCZ  ++ "(" ++ f ++ id++ inpCZ ++ ")) \n"):getCases [] id
                | f == "printRes" = ("\t\tTestCase (assertEqual \"" ++ f ++ "\"" ++ correctPR ++"(" ++ f ++ id ++ inpPR ++ ")) \n"):getCases [] id
                | otherwise = getCases [] id
getCases (f:fs) id | f == "countZeros" = ("\t\tTestCase (assertEqual \"" ++ f ++ "\"" ++ correctCZ ++ "(" ++ f ++ id++ inpCZ ++")), \n"):getCases fs id 
                   | f == "printRes" =  ("\t\tTestCase (assertEqual \"" ++ f ++ "\"" ++ correctPR ++ "(" ++ f ++ id ++ inpCZ ++ ")), \n"):getCases fs id
                   | otherwise = getCases fs id
                     

--            
