-- This is the parser for the CognaC compiler.
-- It processes a Cognate program and outputs a C program,
-- which is then compiled and linked against the runtime located in the include/ directory.

-- Compared to the runtime code, this is pretty messy. Most of the bodges are found here.
-- That is because I eventually plan to rewrite all of this in Cognate itself.

-- TODO: Rewrite all of this in Cognate.

-- Known Parsing Bugs:
--  Strings inside comments are parsed improperly.
--  Only ASCII is supported - not extended ASCII


{-# LANGUAGE LambdaCase #-}

import System.Process
import System.Environment
import Data.List
import Data.Char
import Data.Ratio
import Data.Maybe
import Control.Exception
import Data.List.Split

version = "0.0.1"

replace from to = intercalate to . splitOn from

data Tree =
  Leaf String | Node [Tree]
  deriving (Eq, Show)

parsefile :: String -> [Tree]
-- Let's be honest, most of the parsing functions are obselete now.
parsefile = -- Parsefile takes a string (the file text) as an argument and returns a list of strings (tokens) in the file
  parsesemicolons . -- Reverse sections bounded by semicolons
  parsebrackets . -- Convert bracketed sections to nodes
  map (Leaf . parsenumbers) .
  words . -- Separate into tokens
  parseinformalsyntax .
  map parsesymbols .
  replacesymbols .
  padtokens . -- Space out special characters.
  -- unwords $ parsecharacters $ splitOn "\'" $ -- Convert characters to ASCII value integers
  parseblockcomments . 
  parselinecomments .
  parsestrings . -- Convert strings to lists of characters
  filterAscii

filterAscii :: String -> String
filterAscii str
  | all (\c -> ord c < 128) str = str
  | otherwise = error "Parse Error: ASCII only!"

parselinecomments :: String -> String
parselinecomments str =
  let strings = splitOn "~~" str in
    unwords $ head strings : map (dropWhile (/= '\n')) (tail strings)

parseblockcomments :: String -> String
parseblockcomments = unwords . dropEvens . splitOn "~"
  where dropEvens (x : y : xs) = x : dropEvens xs
        dropEvens x = x

replacesymbols =
  unwords .
  replace [ "==" ] [ "Equal"          ] .
  replace [ "!=" ] [ "Unequal"        ] .
  replace [ "<"  ] [ "Preceed"        ] .
  replace [ ">"  ] [ "Exceed"         ] .
  replace [ ">=" ] [ "EqualOrExceed"  ] .
  replace [ "<=" ] [ "EqualOrPreceed" ] .
  replace [ "+"  ] [ "Sum"            ] .
  replace [ "-"  ] [ "Difference"     ] .
  replace [ "*"  ] [ "Product"        ] .
  replace [ "/"  ] [ "Divisor"        ] .
  words


-- Constants!!!
openbrackets  = [ '{' ]
closebrackets = [ '}' ]
delims        = [ ';',','     ]
numbers       =  '-':'.':['0'..'9']
upperletters  = [ 'A'..'Z'    ]
lowerletters  = [ 'a'..'z'    ]
whitespace    = [ ' ', '\t', '\n' ]
brackets      = openbrackets ++ closebrackets
permittedsymbols = delims    ++ brackets      ++ numbers ++ lowerletters ++ upperletters
formalsymbols = delims       ++ brackets      ++ numbers ++ upperletters

{- parsestrings :: [String] -> [String]
parsestrings (x:y:xs) =
  x : "Tuple" : intercalate " Tuple " (init str) : last str : parsestrings xs
    where str = map (\s -> " \'" ++ [s] ++ "\'") y
parsestrings x = x  -}

parsestrings :: String -> String
parsestrings =
  createStrings . splitOn "'" . replace "\\'" "¸" -- Another string substitution bodge.
    where
      createStrings :: [String] -> String
      createStrings (x:y:xs) =
        x ++ " StringLiteral " ++ [head openbrackets] ++ intercalate [head delims] (map (show . ord) y) ++ [head closebrackets] ++ " " ++ createStrings xs
      createStrings [s] = s
      createStrings [] = ""

{- parsecharacters :: [String] -> [String]
parsecharacters (x:y:xs) =
  x : " Symbol " : show (ord $ head y) : parsecharacters xs
parsecharacters x = x -}

padtokens :: String -> String
padtokens =
  pad (brackets ++ delims)
    where
      pad :: String -> String -> String
      pad (x:xs) str =
        pad xs $
        replace [x] (' ' : x : " ") str
      pad _ x = x

parsesymbols :: Char -> Char
parsesymbols str -- Remove all those pesky symbols.
  | str `elem` whitespace = ' '
  | str `elem` permittedsymbols = str
  | otherwise = '_' -- Convert unusable symbols to underscores.

parsenumbers :: String -> String
parsenumbers (x:xs)
  | x `elem` ('-':'.':numbers) = x : filter (`elem` ('.':numbers)) xs
  | otherwise = x : xs

-- <Bodge>
parsebrackets :: [Tree] -> [Tree]

parsebrackets tokens =
  if closepos /= -1 then -- If there are brackets present, convert them to a list and recur.
    parsebrackets
    (take openpos tokens ++
      [Node
        (take (closepos - openpos - 1)
      (drop (openpos + 1) tokens))] ++
        drop (closepos + 1) tokens)
  else tokens

  where
    leaves = -- Tokens without nodes (allowing me to search it like a normal list)
      map -- Map a function that returns leaves but converts nodes to empty strings onto tokens, returning a [String] for easy processing
      (\case
          Leaf y ->
            y
          Node _ ->
            "")
            tokens

    closepos = -- 1st close bracket.
      fromMaybe (-1) (findIndex (`elem` map (:[]) closebrackets) leaves)

    openpos = -- Last open bracket before closepos.
      case findIndex (`elem` map (:[]) openbrackets) (reverse (take closepos leaves)) of
        Just x ->
          closepos - x - 1
        Nothing ->
          -1
-- </bodge> If it ain't broke, don't fix it.

parsesemicolons :: [Tree] -> [Tree]

parsesemicolons =
  map nodes .
  concatMap reverse .
    splitWhen semicolon
  where
    nodes (Node x) = Node $ parsesemicolons x
    nodes       x  = x
    semicolon (Leaf (x:xs)) 
      | x `elem` delims = True
      | otherwise       = False
    semicolon _         = False

parseinformalsyntax :: String -> String
parseinformalsyntax =
  unwords . filter isformal . words
    where
      isformal word =
        head word `elem` formalsymbols


compile :: [Tree] -> String

doesCall :: [Tree] -> String -> Bool
expr `doesCall` func = func `elem` flatten expr

callCount :: [Tree] -> String -> Int
expr `callCount` func = length $ func `elemIndices` flatten expr

doesMutate :: [Tree] -> String -> Bool
(Leaf var  : Leaf "Set" : xs) `doesMutate` var'  = var       == var'       || xs `doesMutate` var'
(Node func : Leaf "Set" : xs) `doesMutate` func' = last func == Leaf func' || xs `doesMutate` func'
(Node blk : xs) `doesMutate` var' = blk `doesMutate` var' || xs `doesMutate` var'
(_ : xs) `doesMutate` var' = xs `doesMutate` var'
_ `doesMutate` _ = False

flatten :: [Tree] -> [String]
flatten (Node x : xs) = flatten x ++ flatten xs
flatten (Leaf x : xs) = x : flatten xs
flatten [] = []

constructStr :: [Tree] -> String
constructStr str =
  sanitise (map (chr . readNumber) str)
    where 
      readNumber :: Tree -> Int
      readNumber (Leaf num) = read num
      readNumber (Node _) = error "Parse Error: Cannot parse malformed string literal!"
      sanitise = 
        replace "\"" "\\\"" .
        replace "\\'" "'" .
        replace "¸" "'"



compile (Node body : Leaf name : Leaf "Record" : xs) =
  "record(" ++ lc name ++ ", " ++ show (recordSize body) ++ ");\n" ++
  makeFields body ++ "{" ++ compile xs ++ "}"
    where
      makeFields (Node p : Leaf s : xs) = "field(" ++ lc s ++ ", ^{" ++ compile p ++ "});" ++ makeFields xs
      makeFields (Leaf s : xs) = "field(" ++ lc s ++ ", NULL);" ++ makeFields xs
      makeFields [] = ""
      recordSize (Leaf _ : xs) = 1 + recordSize xs
      recordSize (Node _ : xs) =     recordSize xs
      recordSize [] = 0




compile (Node body : Leaf name : Leaf "Define" : xs) =
  -- Defines immutable and nonrecursive if function does not refer to itself in its body and 'Set' is not found in xs.
  -- TODO: Search for 'Set (Name...)' as opposed to just 'Set'.
  if xs `doesCall` name then
    "function(" 
      ++ lc name ++ ", " 
      ++ (if xs `doesMutate` name || body `doesCall` name then "mutable," else "immutable,") 
      ++ (if any isNode body then "copy," else "nocopy,") 
      ++ "{" 
        ++ compile body
        ++ "});{"
    ++ compile xs ++
    "}\n" 
  else compile xs

{-
compile (Node body : Node call : Leaf "Set" : xs) =
  let name = last call
      args = init call in
  "mutate_function(" 
    ++ lc (case name of 
            Leaf str -> str ++ ","
            _        -> error "Parse Error: Invalid function name!") 
    ++ (if any isNode body then "copy, {" else "nocopy, {")
    ++ compile (intersperse (Leaf "Let") (reverse args) ++ [Leaf "Let" | not (null args)] ++ body)
    ++ "});\n{\n"
  ++ compile xs ++
  "}\n"
-}

-- Bind is more elegant, but cannot reccur. Maybe a compromise, where the function is defined at the start of the current block.
{- compile (Leaf name : Leaf "Bind" : xs) = "void(^cognate_" ++ lc name
  ++ ")(void)=pop(block);"
  ++ compile xs 
-}

compile (Leaf name : Leaf "Let" : xs) =
  -- Var is marked as immutable if xs does not contain 'Set'. TODO: mark var as immutable if xs does not contain 'Set Var'
  if xs `doesCall` name then "variable(" ++ lc name ++ ","
  ++ (if xs `doesMutate` name then "mutable" else "immutable")++ ");{"
  ++ compile xs ++ "}" else compile xs

compile (Leaf name : Leaf "Set" : xs) =
  "mutate_variable(" ++ lc name ++ ");{" 
  ++ compile xs ++ "}"



-- Primitive Do inlining.
compile (Node expr : Leaf "Do" : xs) =
  "{\n" ++
    compile expr ++
  "}\n" ++
  compile xs

compile (Node str : Leaf "StringLiteral" : xs) =
  "push(string,\"" ++ constructStr str ++ "\");" ++ compile xs
    
compile (Node expr : xs) =
  "push(block,\nmake_block(" ++ (if any isNode expr then "copy," else "nocopy,") ++ "{\n"
  ++ compile expr
  ++ "}));\n"
  ++ compile xs


compile (Leaf token : xs)
  | all (`elem` ('.':'-':['0'..'9'])) token = "push(number," ++ token ++ ");\n" ++ compile xs
  | otherwise = "call(" ++ lc token ++ ");\n" ++ compile xs

compile [] = ""

compiler = "clang"

isLeaf (Leaf _) = True
isLeaf (Node _) = False

isNode (Node _) = True
isNode (Leaf _) = False


formatFlags = ["-i"]
formatter = "clang-format"

getPath =
  intercalate "/" . init . splitOn "/" <$> getExecutablePath


header in_file = "// Compiled from " ++ in_file ++ " by CognaC version " ++ version ++ "\n"

main :: IO ()
main =
  do
    args <- getArgs
    let compilerFlags = 
          words "-fblocks -lBlocksRuntime -l:libgc.so -Wall -Wno-unused -Ofast -s -I include"
    let in_file = head args
    let out_file = head (splitOn "." in_file) ++ ".c"
    let compiler_args = tail args
    if not (".cog" `isSuffixOf` in_file) then error "Parse Error: Source file must end with .cog file extension" 
    else do
      putStrLn "   ______                        ______\n  / ____/___  ____ _____  ____ _/ ____/\n / /   / __ \\/ __ `/ __ \\/ __ `/ /\n/ /___/ /_/ / /_/ / / / / /_/ / /___\n\\____/\\____/\\__, /_/ /_/\\__,_/\\____/\n            /____/"
      putStrLn $ "Cognate Compiler - Version " ++ version
      putStrLn $ "Compiling " ++ in_file ++ " to " ++ out_file ++ "... "
      source <- readFile in_file
      writeFile out_file $ header in_file ++ "#include\"cognate.c\"\nprogram({" ++ compile (parsefile source) ++ "})"
      rawSystem formatter (formatFlags ++ [out_file])
      putStrLn $ "Compiling " ++ out_file ++ " to " ++ stripExtension in_file ++ "... "
      rawSystem compiler ([out_file, "-o", stripExtension in_file] ++ compilerFlags ++ compiler_args)
      putStrLn "Done!"
      return ()

stripExtension = head . splitOn "."
lc = map toLower
