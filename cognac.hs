-- This is the parser for the CognaC compiler.
-- It processes a Cognate program and outputs a C program,
-- which is then compiled and linked against the runtime located in the include/ directory.

-- Compared to the runtime code, this is pretty messy. Most of the bodges are found here.
-- That is because I eventually plan to rewrite all of this in Cognate itself.

-- TODO: Rewrite all of this in Cognate.

-- Known Parsing Bugs:
--  Strings inside comments are parsed improperly.
--  \nnn and \xhh escape sequences in strings are not caught.


{-# LANGUAGE LambdaCase #-}


import System.Process
import System.Environment
import System.Info
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
  filterAscii .
  parseblockcomments .
  parselinecomments .
  parsestrings -- Convert strings to lists of characters

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
  replace [ "-"  ] [ "Subtract"       ] .
  replace [ "*"  ] [ "Multiply"       ] .
  replace [ "/"  ] [ "Divide"         ] .
  words


-- Constants!!!
openbrackets  = [ '('                 ]
closebrackets = [ ')'                 ]
delims        = [ ';',','             ]
upperletters  = [ 'A'..'Z'            ]
lowerletters  = [ 'a'..'z'            ]
whitespace    = [ ' ', '\t', '\n'     ]
numbers       =  '.' : '-' : ['0'..'9']
brackets      = openbrackets ++ closebrackets
permittedsymbols = delims ++ brackets ++ numbers ++ lowerletters ++ upperletters
formalsymbols =    delims ++ brackets ++ numbers ++ upperletters

parsenumbers :: String -> String

parsenumbers ('-':xs)
  | parseAfterMinus xs /= "" = '-' : parseAfterMinus xs
  | otherwise = ""

parsenumbers ('.':xs)
  | parseAfterDecimal xs /= "" = '.' : parseAfterDecimal xs
  | otherwise = ""

parsenumbers (x:xs)
  | x `elem` ['0'..'9'] = x : parsenumbers xs
  | otherwise = x : xs

parsenumbers _ = ""

parseAfterMinus ('.':xs)
  | parseAfterDecimal xs /= "" = '.' : parseAfterDecimal xs
  | otherwise = ""

parseAfterMinus (x:xs)
  | x `elem` ['0'..'9'] = x : parseAfterMinus xs
  | otherwise = ""

parseAfterMinus _ = ""

parseAfterDecimal (x:xs)
  | x `elem` ['0'..'9'] = x : parseAfterDecimal xs
  | otherwise = ""

parseAfterDecimal _ = ""


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


parseImports :: String -> [Tree] -> [String] -> IO [Tree]

-- TODO: Use strings to store import filepath, relative to currently compiling filepath.
parseImports path (Leaf filename : Leaf "Import" : xs) imported =
    -- Don't import if its already been imported.
    if (case findIndex (== filename) imported of
          Just x -> True
          Nothing -> False)
    then
      parseImports path xs imported
    else do
      putStrLn $ "Importing " ++ filename ++ " from " ++ filename ++ ".cog"
      importedFile <- readFile ((join "/" $ init $ splitOn "/" path) ++ (if (length (splitOn "/" path) > 1) then "/" else "") ++ filename ++ ".cog")
      xs' <- parseImports path xs (imported ++ [filename])
      x   <- parseImports path (parsefile importedFile) (imported ++ [filename])
      return $ x ++ [Node xs',Leaf "Do"]
        where
          join :: String -> [String] -> String
          join delim (x:xs) = x ++ delim ++ join delim xs
          join _ [] = []

parseImports path (Node x : xs) imported =
  do
    x'  <- parseImports path x imported
    xs' <- parseImports path xs imported
    return $ Node x' : xs'

parseImports path (x:xs) imported =
  do
    xs' <- parseImports path xs imported
    return $ x : xs'

parseImports _ [] _ = return []

compile :: [Tree] -> [Tree] -> String

doesCall :: [Tree] -> String -> Bool

num_args "if" = 3
num_args "while" = 2
num_args "do" = 1
num_args "put" = 1
num_args "print" = 1
num_args "sum" = 2
num_args "multiply" = 2
num_args "divide" = 2
num_args "subtract" = 2
num_args "modulo" = 2
num_args "random" = 3
num_args "drop" = 1
num_args "twin" = 1
num_args "triplet" = 1
num_args "swap" = 2
num_args "clear" = 0
num_args "true" = 0
num_args "false" = 0
num_args "either" = 2
num_args "both" = 2
num_args "one_of" = 2
num_args "not" = 1
num_args "equal" = 2
num_args "unequal" = 2
num_args "preceed" = 2
num_args "exceed" = 2
num_args "equalorexceed" = 2
num_args "number_" = 1
num_args "list_" = 1
num_args "string_" = 1
num_args "block_" = 1
num_args "boolean_" = 1
num_args "head" = 1
num_args "tail" = 1
num_args "push" = 2
num_args "empty" = 1
num_args "list" = 1
num_args "suffix" = 2
num_args "string_length" = 1
num_args "substring" = 3
num_args "input" = 0
num_args "read" = 1
num_args "number" = 1
num_args "path" = 0
num_args "stack" = 0
num_args "write" = 2
num_args "parameters" = 0
num_args "stop" = 0
num_args "table" = 1
num_args "insert" = 2
num_args "values" = 1
num_args "match" = 2
num_args "ordinal" = 1
num_args "character" = 1
num_args "floor" = 1
num_args "round" = 1
num_args "ceiling" = 1
num_args "assert" = 2
num_args "error" = 1
num_args _ = 0

(Node body : Leaf name : Leaf "Define" : xs) `doesCall` func
  | xs `doesCall` name = xs `doesCall` func || body `doesCall` func
  | otherwise = xs `doesCall` func

(Leaf str : xs) `doesCall` func
  | str == func = True
  | otherwise = xs `doesCall` func

(Node x : xs) `doesCall` func = xs `doesCall` func || x `doesCall` func

_ `doesCall` _ = False


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
      sanitise str
        | "\\?" `isInfixOf` str = error "Invalid Escape Character \\?"
        | "\\\"" `isInfixOf` str = error "Invalid Escape Character \\\""
        | "\\0" `isInfixOf` str = error "Invalid Escape Character \\0"
        | otherwise = replace "\"" "\\\"" $ replace "¸" "'" str


{-
compile (Leaf "" : xs) = "" ++ compile xs


compile (Node body : Leaf name : Leaf "Define" : xs) =
  -- Defines immutable and nonrecursive if function does not refer to itself in its body and 'Set' is not found in xs.
  if xs `doesCall` name then
    "function("
      ++ lc name ++ ", "
      ++ (if xs `doesMutate` name || body `doesCall` name then "mutable," else "immutable,")
      ++ (if any isNode body then "1," else "0,")
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
    ++ (if any isNode body then "1, {" else "0, {")
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
  "variable(" ++ lc name ++ ","
  ++ (if xs `doesMutate` name then "mutable" else "immutable")++ ");{"
  ++ compile xs ++ "}"

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
  "push(block,\nmake_block(" ++ (if any isNode expr then "1," else "0,") ++ "{\n"
  ++ compile expr
  ++ "}));\n"
  ++ compile xs


compile (Leaf token : xs)
  | all (`elem` ('.':'-':['0'..'9'])) token = "push(number," ++ token ++ ");\n" ++ compile xs
  | otherwise = "call(" ++ lc token ++ ");\n" ++ compile xs

compile [] = ""

-}

make_obj :: Tree -> String
make_obj a = "OBJ(" ++ literal_type a ++ "," ++ print_literal a ++ ")"

literal_type :: Tree -> String

literal_type (Leaf token)
  | all (`elem` ('.':'-':['0'..'9'])) token = "number"
  | head token == '\"' = "string"

literal_type (Node token) = "block"

print_literal :: Tree -> String
print_literal (Leaf str) = str
print_literal (Node blk) = "make_block(0, {" ++ compile blk [] ++ "})"

stack_push :: Tree -> String

stack_push a = "push_any(" ++ make_obj a ++ ");"

is_literal :: String -> Bool
is_literal str = not $ head str `elem` upperletters

-- TODO:
-- Simple type inference so we can pass arguments as values instead of complete cognate_objects.
-- Inlining stack pushes as function returns seemed to hurt performance (probably because sizeof(cognate_object) > 8), but it could help with type inference where we can pass smaller values.
-- Inline arguments to user definied functions where Let expressions are at the start [remember not to break error messages].
-- Peephole optimizations, such as eliminating Drop expressions.
-- Rewrite the entire parser in Cognate ASAP.

compile (Node blk:xs) buf = compile xs (Node blk:buf)
compile (Leaf "" : xs) buf =compile xs buf
compile (Leaf "StringLiteral":xs) (Node str:xss) = compile xs (Leaf ("\"" ++ constructStr str ++ "\"") : xss)
compile (Leaf str : Leaf "Define" : xs) (Node blk : xss) = if xs `doesCall` str then "function(" ++ lc str ++ (if blk `doesCall` str then ",mutable," else ",immutable,") ++ "0,{" ++ compile blk [] ++ "}); {" ++ compile xs xss ++ "}" else compile xs xss
compile (Leaf str : Leaf "Let" : xs) (value:buf) = "variable(" ++ lc str ++ "," ++ (if xs `doesMutate` str then "mutable" else "immutable") ++ ", " ++ make_obj value ++ ");{" ++ compile xs buf ++"}"
compile (Leaf str : Leaf "Let" : xs) buf = "variable(" ++ lc str ++ "," ++ (if xs `doesMutate` str then "mutable" else "immutable") ++ ", pop_any());{" ++ compile xs buf ++"}"
compile (Leaf str:xs) buf
  | is_literal str = compile xs (Leaf str:buf)
  | otherwise = (intercalate " " $ map stack_push (reverse (drop args buf))) ++ "call(" ++ lc str ++ ",(" ++ (intercalate "," $ (map make_obj $ take args buf) ++ (if (length buf < args) then (take (args - length buf) $ cycle ["pop_any()"]) else [])) ++ "));" ++ (compile xs [])
  where args = num_args $ lc str


compile [] buf = intercalate " " $ map stack_push $ reverse buf


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
    let compilerFlagsLinux =
          words "-fblocks -lBlocksRuntime -l:libgc.so -Ofast -I include -Wall -Wextra -Werror -Wno-unused -pedantic-errors -std=c11 -lm -g0 -rdynamic -fuse-ld=lld -Wno-gnu-zero-variadic-macro-arguments"
    let compilerFlagsMac =
          words "-fblocks -lgc -Ofast -I include -Wall -Wextra -Werror -pedantic-errors -Wno-unused -std=c11 -lm -g0 -rdynamic -fuse-ld=lld -Wno-gnu-zero-variadic-macro-arguments"
    let compilerFlags = if System.Info.os == "linux" then compilerFlagsLinux else compilerFlagsMac
    let in_file = head args
    let out_file = head (splitOn "." in_file) ++ ".c"
    let compiler_args = tail args
    if not (".cog" `isSuffixOf` in_file) then error "Parse Error: Source file must end with .cog file extension"
    else do
      putStrLn "   ______                        ______\n  / ____/___  ____  ____  ____  / ____/\n / /   / __ \\/ __ `/ __ \\/ __ `/ /\n/ /___/ /_/ / /_/ / / / / /_/ / /___\n\\____/\\____/\\__, /_/ /_/\\__,_/\\____/\n           /____/"
      putStrLn $ "Cognate Compiler - Version " ++ version
      putStrLn $ "Compiling " ++ in_file ++ " to " ++ out_file ++ "... "
      source <- readFile in_file
      thing <- parseImports in_file (parsefile source) [head $ splitOn "." (last (splitOn "/" in_file))]
      writeFile out_file $ header in_file ++ "#include\"cognate.c\"\nprogram({" ++ compile thing [] ++ "})\n"
      rawSystem formatter (formatFlags ++ [out_file])
      putStrLn $ "Compiling " ++ out_file ++ " to " ++ stripExtension in_file ++ "... "
      rawSystem compiler ([out_file, "-o", stripExtension in_file] ++ compilerFlags ++ compiler_args)
      putStrLn "Done!"
      return ()

stripExtension = head . splitOn "."
lc = map toLower
