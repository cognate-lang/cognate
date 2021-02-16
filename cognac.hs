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

import Control.Exception
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ratio
import System.Environment
import System.Info
import System.Process

version = "0.0.1"

replace from to = intercalate to . splitOn from

data Tree
  = Leaf String
  | Node [Tree]
  deriving (Eq, Show)

parsefile :: String -> [Tree]
-- Let's be honest, most of the parsing functions are obselete now.
parsefile -- Parsefile takes a string (the file text) as an argument and returns a list of strings (tokens) in the file
 =
  parsesemicolons . -- Reverse sections bounded by semicolons
  parsebrackets . -- Convert bracketed sections to nodes
  map (Leaf . parsenumbers) .
  words . -- Separate into tokens
  parseinformalsyntax .
  map parsesymbols .
  replacesymbols .
  padtokens . -- Space out special characters.
  -- unwords $ parsecharacters $ splitOn "\'" $ -- Convert characters to ASCII value integers
  filterAscii . parseblockcomments . parselinecomments . parsestrings

filterAscii :: String -> String
filterAscii str
  | all (\c -> ord c < 128) str = str
  | otherwise = error "Parse Error: ASCII only!"

parselinecomments :: String -> String
parselinecomments str =
  let strings = splitOn "~~" str
   in unwords $ head strings : map (dropWhile (/= '\n')) (tail strings)

parseblockcomments :: String -> String
parseblockcomments = unwords . dropEvens . splitOn "~"
  where
    dropEvens (x:y:xs) = x : dropEvens xs
    dropEvens x = x

replacesymbols =
  unwords .
  replace ["=="] ["Equal"] .
  replace ["!="] ["Unequal"] .
  replace ["<"] ["Preceed"] .
  replace [">"] ["Exceed"] .
  replace [">="] ["EqualOrExceed"] .
  replace ["<="] ["EqualOrPreceed"] .
  replace ["+"] ["Sum"] .
  replace ["-"] ["Subtract"] .
  replace ["*"] ["Multiply"] . replace ["/"] ["Divide"] . words

-- Constants!!!
openbrackets = ['(']

closebrackets = [')']

delims = [';', ',']

upperletters = ['A' .. 'Z']

lowerletters = ['a' .. 'z']

whitespace = [' ', '\t', '\n']

numbers = '.' : '-' : ['0' .. '9']

brackets = openbrackets ++ closebrackets

permittedsymbols = delims ++ brackets ++ numbers ++ lowerletters ++ upperletters

formalsymbols = delims ++ brackets ++ numbers ++ upperletters

parsenumbers :: String -> String
parsenumbers ('-':xs)
  | parseAfterMinus xs /= "" = '-' : parseAfterMinus xs
  | otherwise = ""
parsenumbers ('.':xs)
  | parseAfterDecimal xs /= "" = '.' : parseAfterDecimal xs
  | otherwise = ""
parsenumbers (x:xs)
  | x `elem` ['0' .. '9'] = x : parsenumbers xs
  | otherwise = x : xs
parsenumbers _ = ""

parseAfterMinus ('.':xs)
  | parseAfterDecimal xs /= "" = '.' : parseAfterDecimal xs
  | otherwise = ""
parseAfterMinus (x:xs)
  | x `elem` ['0' .. '9'] = x : parseAfterMinus xs
  | otherwise = ""
parseAfterMinus _ = ""

parseAfterDecimal (x:xs)
  | x `elem` ['0' .. '9'] = x : parseAfterDecimal xs
  | otherwise = ""
parseAfterDecimal _ = ""

parsestrings :: String -> String
parsestrings = -- FIXME: string '\\' breaks here
  createStrings . splitOn "'" . replace "\\'" "¸" -- Another string substitution bodge.
  where
    createStrings :: [String] -> String
    createStrings (x:y:xs) =
      x ++
      " StringLiteral " ++
      [head openbrackets] ++
      intercalate [head delims] (map (show . ord) y) ++
      [head closebrackets] ++ " " ++ createStrings xs
    createStrings [s] = s
    createStrings [] = ""

{- parsecharacters :: [String] -> [String]
parsecharacters (x:y:xs) =
  x : " Symbol " : show (ord $ head y) : parsecharacters xs
parsecharacters x = x -}
padtokens :: String -> String
padtokens = pad (brackets ++ delims)
  where
    pad :: String -> String -> String
    pad (x:xs) str = pad xs $ replace [x] (' ' : x : " ") str
    pad _ x = x

parsesymbols :: Char -> Char
parsesymbols str -- Remove all those pesky symbols.
  | str `elem` whitespace = ' '
  | str `elem` permittedsymbols = str
  | otherwise = '_' -- Convert unusable symbols to underscores.

-- <Bodge>
parsebrackets :: [Tree] -> [Tree]
parsebrackets tokens =
  if closepos /= -1 -- If there are brackets present, convert them to a list and recur.
    then parsebrackets
           (take openpos tokens ++
            [Node (take (closepos - openpos - 1) (drop (openpos + 1) tokens))] ++
            drop (closepos + 1) tokens)
    else tokens
  where
    leaves -- Tokens without nodes (allowing me to search it like a normal list)
     =
      map -- Map a function that returns leaves but converts nodes to empty strings onto tokens, returning a [String] for easy processing
        (\case
           Leaf y -> y
           Node _ -> "")
        tokens
    closepos -- 1st close bracket.
     = fromMaybe (-1) (findIndex (`elem` map (: []) closebrackets) leaves)
    openpos -- Last open bracket before closepos.
     =
      case findIndex
             (`elem` map (: []) openbrackets)
             (reverse (take closepos leaves)) of
        Just x -> closepos - x - 1
        Nothing -> -1

-- </bodge> If it ain't broke, don't fix it.
parsesemicolons :: [Tree] -> [Tree]
parsesemicolons = map nodes . concatMap reverse . splitWhen semicolon
  where
    nodes (Node x) = Node $ parsesemicolons x
    nodes x = x
    semicolon (Leaf (x:xs))
      | x `elem` delims = True
      | otherwise = False
    semicolon _ = False

parseinformalsyntax :: String -> String
parseinformalsyntax = unwords . filter isformal . words
  where
    isformal word = head word `elem` formalsymbols

parseImports :: String -> [Tree] -> [String] -> IO [Tree]
-- TODO: Use strings to store import filepath, relative to currently compiling filepath.
parseImports path (Leaf filename:Leaf "Import":xs) imported
    -- Don't import if its already been imported.
 =
  if (case findIndex (== filename) imported of
        Just x -> True
        Nothing -> False)
    then parseImports path xs imported
    else do
      putStrLn $ "Importing " ++ filename ++ " from " ++ filename ++ ".cog"
      importedFile <-
        readFile
          ((join "/" $ init $ splitOn "/" path) ++
           (if (length (splitOn "/" path) > 1)
              then "/"
              else "") ++
           filename ++ ".cog")
      xs' <- parseImports path xs (imported ++ [filename])
      x <- parseImports path (parsefile importedFile) (imported ++ [filename])
      return $ x ++ [Node xs', Leaf "Do"]
  where
    join :: String -> [String] -> String
    join delim (x:xs) = x ++ delim ++ join delim xs
    join _ [] = []
parseImports path (Node x:xs) imported = do
  x' <- parseImports path x imported
  xs' <- parseImports path xs imported
  return $ Node x' : xs'
parseImports path (x:xs) imported = do
  xs' <- parseImports path xs imported
  return $ x : xs'
parseImports _ [] _ = return []

compile :: [Tree] -> [Tree] -> [String] -> String
doesCall :: [Tree] -> String -> Bool
args "if" = ["block", "", ""]
args "while" = ["block", "block"]
args "do" = ["block"]
args "put" = [""]
args "print" = [""]
args "sum" = ["number", "number"]
args "multiply" = ["number", "number"]
args "divide" = ["number", "number"]
args "subtract" = ["number", "number"]
args "modulo" = ["number", "number"]
args "random" = ["number", "number", "number"]
args "drop" = [""]
args "twin" = [""]
args "triplet" = [""]
args "swap" = ["", ""]
args "clear" = []
args "true" = []
args "false" = []
args "either" = ["boolean", "boolean"]
args "both" = ["boolean", "boolean"]
args "one_of" = ["boolean", "boolean"]
args "not" = ["boolean"]
args "equal" = ["", ""]
args "unequal" = ["", ""]
args "preceed" = ["number", "number"]
args "exceed" = ["number", "number"]
args "equalorexceed" = ["number", "number"]
args "number_" = [""]
args "list_" = [""]
args "string_" = [""]
args "block_" = [""]
args "boolean_" = [""]
args "first" = ["list"]
args "rest" = ["list"]
args "head" = ["string"]
args "tail" = ["string"]
args "push" = ["", "list"]
args "empty_" = [""]
args "list" = ["block"]
args "join" = ["number"]
args "string_length" = ["string"]
args "substring" = ["number", "number", "string"]
args "input" = []
args "read" = ["string"]
args "number" = ["string"]
args "path" = []
args "stack" = []
args "write" = ["string", ""]
args "parameters" = []
args "stop" = []
args "table" = ["block"]
args "insert" = ["string", "", "block"]
args "values" = ["table"]
args "match" = ["string", "string"]
args "ordinal" = ["string"]
args "character" = ["number"]
args "floor" = ["number"]
args "round" = ["number"]
args "ceiling" = ["number"]
args "assert" = ["string", "boolean"]
args "error" = ["string"]
args _ = []

ret "if" = "" -- Functions returning objects should use the stack, it's actually faster.
ret "while" = ""
ret "do" = ""
ret "put" = ""
ret "print" = ""
ret "sum" = "number"
ret "multiply" = "number"
ret "divide" = "number"
ret "subtract" = "number"
ret "modulo" = "number"
ret "random" = "number"
ret "drop" = ""
ret "twin" = ""
ret "triplet" = ""
ret "swap" = ""
ret "clear" = ""
ret "true" = "boolean"
ret "false" = "boolean"
ret "either" = "boolean"
ret "both" = "boolean"
ret "one_of" = "boolean"
ret "not" = "boolean"
ret "equal" = "boolean"
ret "unequal" = "boolean"
ret "preceed" = "boolean"
ret "exceed" = "boolean"
ret "equalorexceed" = "boolean"
ret "number_" = "boolean"
ret "list_" = "boolean"
ret "string_" = "boolean"
ret "block_" = "boolean"
ret "boolean_" = "boolean"
ret "first" = ""
ret "rest" = "list"
ret "head" = "string"
ret "tail" = "string"
ret "push" = "list"
ret "empty_" = "boolean"
ret "list" = "list"
ret "join" = "string"
ret "string_length" = "number"
ret "substring" = "string"
ret "input" = "string"
ret "read" = "string"
ret "number" = "number"
ret "path" = "string"
ret "stack" = ""
ret "write" = ""
ret "parameters" = "list"
ret "stop" = ""
ret "table" = "table"
ret "insert" = "table"
ret "values" = "list"
ret "match" = "boolean"
ret "ordinal" = "number"
ret "character" = "string"
ret "floor" = "number"
ret "round" = "number"
ret "ceiling" = "number"
ret "assert" = ""
ret "error" = ""
ret _ = ""

-- FIXME: Functions shadowing these built in functions WILL BREAK.
(Node body:Leaf name:Leaf "Define":xs) `doesCall` func
  | xs `doesCall` name = xs `doesCall` func || body `doesCall` func
  | otherwise = xs `doesCall` func
(Leaf str:xs) `doesCall` func
  | lc str == lc func = True
  | otherwise = xs `doesCall` func
(Node x:xs) `doesCall` func = xs `doesCall` func || x `doesCall` func
_ `doesCall` _ = False

callCount :: [Tree] -> String -> Int
expr `callCount` func = length $ func `elemIndices` flatten expr

doesMutate :: [Tree] -> String -> Bool
(Leaf var:Leaf "Set":xs) `doesMutate` var' =
  lc var == lc var' || xs `doesMutate` var'
(Node func:Leaf "Set":xs) `doesMutate` func' =
  (case last func of
     Leaf a -> (Leaf $ lc a)
     Node a -> (Node a)) ==
  Leaf (lc func') ||
  xs `doesMutate` func'
(Node blk:xs) `doesMutate` var' = blk `doesMutate` var' || xs `doesMutate` var'
(_:xs) `doesMutate` var' = xs `doesMutate` var'
_ `doesMutate` _ = False

flatten :: [Tree] -> [String]
flatten (Node x:xs) = flatten x ++ flatten xs
flatten (Leaf x:xs) = x : flatten xs
flatten [] = []

constructStr :: [Tree] -> String
constructStr str = sanitise (map (chr . readNumber) str)
  where
    readNumber :: Tree -> Int
    readNumber (Leaf num) = read num
    readNumber (Node _) =
      error "Parse Error: Cannot parse malformed string literal!"
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
generate_cast :: String -> String
generate_cast "" = "pop()"
generate_cast typ = "CHECK(" ++ typ ++ ",pop())"

chk_type :: (Tree, String) -> [String] -> String
chk_type (obj, typ) vars
  | (literal_type obj) == typ = print_literal obj vars
  | typ == "" = make_obj obj vars
  | literal_type obj == "" =
    "CHECK(" ++ typ ++ "," ++ print_literal obj vars ++ ")"
  | otherwise =
    error $
    "Type error is guaranteed on execution. Expected type " ++
    typ ++ " but got type " ++ literal_type obj

make_obj :: Tree -> [String] -> String
make_obj a vars
  | literal_type a == "" = print_literal a vars
  | otherwise = "OBJ(" ++ literal_type a ++ "," ++ print_literal a vars ++ ")"

literal_type :: Tree -> String
literal_type (Leaf token)
  | all (`elem` ('.' : '-' : ['0' .. '9'])) token = "number"
  | head token == '\"' = "string"
  | "VAR(" `isPrefixOf` token = ""
  | "CALL(" `isPrefixOf` token = ret (takeWhile (/= ',') (drop 5 token))
  | otherwise = error ("Cannot match '" ++ token ++ "'")
literal_type (Node token) = "block"

print_literal :: Tree -> [String] -> String
print_literal (Leaf str) _ = str
print_literal (Node blk) vars = "BLOCK(" ++ compile blk [] vars ++ ")"

stack_push :: Tree -> [String] -> String
stack_push a vars = "push(" ++ make_obj a vars ++ ");"

is_literal :: String -> Bool
is_literal str = not $ head str `elem` upperletters

check_shadow :: String -> String
check_shadow str =
  if (ret (lc str) /= "" || args (lc str) /= [])
    then error "Cannot shadow/mutate builtin functions yet!"
    else str

-- TODO:
-- Inline arguments to user definied functions where Let expressions are at the start [remember not to break error messages].
-- Peephole optimizations, such as eliminating Drop expressions.
-- Rewrite the entire parser in Cognate ASAP.
compile (Node blk:xs) buf vars = compile xs (Node blk : buf) vars
compile (Leaf "":xs) buf vars = compile xs buf vars
compile (Leaf "StringLiteral":xs) (Node str:xss) vars =
  compile xs (Leaf ("\"" ++ constructStr str ++ "\"") : xss) vars
compile (Leaf str:Leaf "Define":xs) (Node blk:xss) vars = -- TODO: Nondeterministic function definitions are very easily doable here.
  if xs `doesCall` (lc str)
    then "DEFINE(" ++
         (if blk `doesCall` check_shadow (lc str)
            then "mutable,"
            else "immutable,") ++
         lc str ++
         ",{" ++
         compile blk [] (filter (/= lc str) vars) ++
         "}); {" ++ compile xs xss (filter (/= lc str) vars) ++ "}"
    else compile xs xss vars
compile (Leaf str:Leaf "Let":xs) (value:buf) vars =
  "LET(" ++
  (if xs `doesMutate` lc str
     then "mutable"
     else "immutable") ++
  "," ++
  check_shadow (lc str) ++
  "," ++ make_obj value vars ++ ");{" ++ compile xs buf (lc str : vars) ++ "}"
compile (Leaf str:Leaf "Let":xs) buf vars =
  "LET(" ++
  (if xs `doesMutate` (lc str)
     then "mutable"
     else "immutable") ++
  "," ++
  check_shadow (lc str) ++ ",pop());{" ++ compile xs buf (lc str : vars) ++ "}"
compile (Leaf str:Leaf "Set":xs) (value:buf) vars =
  "SET(" ++
  check_shadow (lc str) ++
  "," ++ make_obj value vars ++ ");" ++ compile xs buf vars
compile (Leaf str:Leaf "Set":xs) buf vars =
  "SET(" ++ check_shadow (lc str) ++ ",pop());" ++ compile xs buf vars
compile (Leaf str:xs) buf vars
  | is_literal str = compile xs (Leaf str : buf) vars
  | lc str `elem` vars = compile xs (Leaf ("VAR(" ++ lc str ++ ")") : buf) vars
  | ret (lc str) /= "" = compile xs (Leaf call : drop num_args buf) vars -- FIXME If an IO function returns a value, then this will mess with order of IO. Fix is to empty buff and prepend excess but that degrades performance.
  | otherwise = excess ++ call ++ ";" ++ compile xs [] vars
  where
    num_args = length $ args $ lc str
    call =
      "CALL(" ++
      lc str ++
      ",(" ++
      (intercalate "," $
       (map (\x -> chk_type x vars) (zip (take num_args buf) (args (lc str)))) ++
       (if (length buf < num_args)
          then (take (num_args - length buf) $
                (map generate_cast $ drop (length buf) $ args (lc str)))
          else [])) ++
      "))"
    excess =
      intercalate "" $
      map (\z -> stack_push z vars) (reverse (drop num_args buf))
compile [] buf vars =
  intercalate " " $ map (\x -> stack_push x vars) $ reverse buf

compiler = "clang"

isLeaf (Leaf _) = True
isLeaf (Node _) = False

isNode (Node _) = True
isNode (Leaf _) = False

formatFlags = ["-i"]

formatter = "clang-format"

getPath = intercalate "/" . init . splitOn "/" <$> getExecutablePath

header in_file =
  "// Compiled from " ++ in_file ++ " by CognaC version " ++ version ++ "\n"

main :: IO ()
main = do
  args <- getArgs
  let compilerFlagsLinux =
        words
          "-fblocks -lBlocksRuntime -l:libgc.so -Ofast -I include -Wall -Wextra -Werror -Wno-unused -pedantic-errors -std=c11 -lm -g0 -fuse-ld=lld"
  let compilerFlagsMac =
        words
          "-fblocks -lgc -Ofast -I include -Wall -Wextra -Werror -pedantic-errors -Wno-unused -std=c11 -lm -g0"
  let compilerFlags =
        if System.Info.os == "linux"
          then compilerFlagsLinux
          else compilerFlagsMac
  let in_file = head args
  let out_file = head (splitOn "." in_file) ++ ".c"
  let compiler_args = tail args
  if not (".cog" `isSuffixOf` in_file)
    then error "Parse Error: Source file must end with .cog file extension"
    else do
      putStrLn
        "   ______                        ______\n  / ____/___  ____  ____  ____  / ____/\n / /   / __ \\/ __ `/ __ \\/ __ `/ /\n/ /___/ /_/ / /_/ / / / / /_/ / /___\n\\____/\\____/\\__, /_/ /_/\\__,_/\\____/\n           /____/"
      putStrLn $ "Cognate Compiler - Version " ++ version
      putStrLn $ "Compiling " ++ in_file ++ " to " ++ out_file ++ "... "
      source <- readFile in_file
      thing <-
        parseImports
          in_file
          (parsefile source)
          [head $ splitOn "." (last (splitOn "/" in_file))]
      writeFile out_file $
        header in_file ++
        "#include\"cognate.c\"\nPROGRAM(" ++ compile thing [] [] ++ ")\n"
      --rawSystem formatter (formatFlags ++ [out_file])
      putStrLn $
        "Compiling " ++ out_file ++ " to " ++ stripExtension in_file ++ "... "
      rawSystem
        compiler
        ([out_file, "-o", stripExtension in_file] ++
         compilerFlags ++ compiler_args)
      putStrLn "Done!"
      return ()

stripExtension = head . splitOn "."

lc = map toLower
