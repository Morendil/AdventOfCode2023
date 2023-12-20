import Text.ParserCombinators.ReadP
import Data.Char (isAlpha)
import Data.Maybe (fromJust)
import Data.List (partition)
import Data.List.HT (takeUntil)
import qualified Data.Map as M
import qualified Data.Set as S

type Config = M.Map String (Char,[String])
type Pulse = (String, Bool, String)
data Module = Cast | FlipFlop Bool | Conjunction (S.Set String, S.Set String) deriving (Eq, Show)
type Memory = M.Map String Module
type State = (Memory, [Pulse], [Pulse])

wire :: ReadP (String, (Char, [String]))
wire = do
    kind <- option '!' (choice [char '%', char '&'])
    name <- many1 (satisfy isAlpha)
    string " -> "
    ends <- sepBy1 (many1 (satisfy isAlpha)) (string ", ")
    return (name, (kind,ends))

buildConjunction :: Config -> String -> Module
buildConjunction config name = Conjunction (S.empty, S.fromList origins)
    where origins = M.keys $ M.filter (elem name . snd) config

buildMemory :: Config -> Memory
buildMemory config = M.mapWithKey makeModule config
    where makeModule name ('&',_) = buildConjunction config name
          makeModule name ('!',_) = Cast
          makeModule name ('%',_) = FlipFlop False

processPulse :: Config -> Memory -> Pulse -> (Memory, [Pulse])
processPulse config mem pulse@(from,value,to) = case target of
        Just Cast -> (mem, emit False)
        Just (FlipFlop state) -> if value then noop else (update (FlipFlop (not state)), emit (not state))
        Just (Conjunction (seen, all)) -> (update (Conjunction (seen', all)), emit (seen' /= all))
            where seen' = see from value seen
        Nothing -> noop
    where target = M.lookup to mem
          destinations = snd $ fromJust $ M.lookup to config
          send value dest = (to,value,dest)
          emit value = map (send value) destinations
          noop = (mem, [])
          see from value seen = if value then S.insert from seen else S.delete from seen
          update mod = M.insert to mod mem

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

step :: Config -> State -> State
step config (mem,[],history) = (mem,[],history)
step config (mem,queue,history) = (newMem, tail queue ++ queued, head queue:history)
    where (newMem, queued) = processPulse config mem (head queue)

display :: Pulse -> String
display (from,value,to) = from ++ " -" ++ (if value then "high" else "low") ++ "-> " ++ to

buttonPress :: Pulse
buttonPress = ("button",False,"broadcaster")

pressButton :: State -> State
pressButton (m,[],h) = (m,[buttonPress],h)
pressButton _ = error "Please wait until pulses are processed before pressing button"

stabilize :: Config -> State -> State
stabilize config state = last $ takeUntil (\(_,q,_) -> null q) $ iterate (step config) $ pressButton state

part1 :: Config -> Int
part1 config = length lo * length hi
  where (_,_,history) = last $ take 1001 $ iterate (stabilize config) (buildMemory config, [], [])
        (lo, hi) = let low (_,v,_) = v in partition low history

main = do
    config <- M.fromList . fromJust . parseMaybe (sepBy1 wire (string "\n")) <$> readFile "day20.txt"
    print $ part1 config

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result
