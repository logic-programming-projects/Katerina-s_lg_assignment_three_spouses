{-# LANGUAGE DeriveGeneric #-}

-- Задача «Три подружжя» — пошук у ширину (BFS) мовою Haskell

import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, ViewL(..), (|>))
import Data.List (sort)
import GHC.Generics (Generic)

-- Сторона річки: ліва (L) або права (R)
data Side = L | R deriving (Eq, Ord, Show, Generic)

-- Особи: три чоловіки (H1–H3) та три дружини (W1–W3)
data Person = H1 | H2 | H3 | W1 | W2 | W3
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

-- Берег річки — список осіб
type Bank = [Person]

-- Стан задачі: де знаходиться човен і хто на якому березі
data State = State
  { boat  :: Side
  , leftB :: Bank
  , rightB :: Bank
  } deriving (Eq, Ord, Show, Generic)

-- Усі особи задачі
allPeople :: [Person]
allPeople = [minBound .. maxBound]

-- Канонічне представлення стану:
-- сортуємо береги, щоб еквівалентні стани вважались однаковими
canon :: State -> State
canon (State b l r) = State b (sort l) (sort r)

-- Початковий стан: усі на лівому березі
initialState :: State
initialState = canon $ State L allPeople []

-- Цільовий стан: усі на правому березі
goalState :: State
goalState = canon $ State R [] allPeople

-- Перевірка: чи є особа чоловіком
isH :: Person -> Bool
isH p = p `elem` [H1,H2,H3]

-- Перевірка: чи є особа дружиною
isW :: Person -> Bool
isW p = p `elem` [W1,W2,W3]

-- Визначення чоловіка для кожної дружини
husbandOf :: Person -> Person
husbandOf W1 = H1
husbandOf W2 = H2
husbandOf W3 = H3
husbandOf _  = error "husbandOf викликано не для дружини"

-- Перевірка безпеки берега:
-- якщо на березі є і чоловіки, і дружини,
-- то кожна дружина повинна мати поруч свого чоловіка
safeBank :: Bank -> Bool
safeBank bank =
  let men = filter isH bank
      women = filter isW bank
  in null men || null women ||
     all (\w -> husbandOf w `elem` bank) women

-- Стан безпечний, якщо обидва береги безпечні
safeState :: State -> Bool
safeState (State _ l r) = safeBank l && safeBank r

-- Вибір пасажирів для човна:
-- або одна особа, або дві (без повторів)
passengers :: Bank -> [[Person]]
passengers bank =
  [[p] | p <- bank] ++
  [[p,q] | (i,p) <- indexed bank
         , q <- drop (i+1) bank]
  where
    indexed xs = zip [0..] xs

-- Видалення осіб з берега
removePeople :: [Person] -> Bank -> Bank
removePeople ps bank = foldl (flip removeOne) bank ps
  where
    removeOne x xs = case break (==x) xs of
      (a, _ : b) -> a ++ b
      _          -> xs

-- Додавання осіб на берег
addPeople :: [Person] -> Bank -> Bank
addPeople ps bank = sort (ps ++ bank)

-- Генерація сусідніх станів (один крок переправи)
neighbors :: State -> [State]
neighbors s@(State L l r) =
  [ canon next
  | ps <- passengers l
  , let l1 = removePeople ps l
  , let r1 = addPeople ps r
  , let next = State R l1 r1
  , safeState next
  ]
neighbors s@(State R l r) =
  [ canon next
  | ps <- passengers r
  , let r1 = removePeople ps r
  , let l1 = addPeople ps l
  , let next = State L l1 r1
  , safeState next
  ]

-- Пошук у ширину (BFS), який повертає шлях станів
bfs :: State -> Maybe [State]
bfs start = go (Seq.singleton [start]) (Set.singleton start)
  where
    go :: Seq [State] -> Set.Set State -> Maybe [State]
    go q visited =
      case Seq.viewl q of
        EmptyL -> Nothing
        path Seq.:< q1 ->
          let s = head path
          in if s == goalState
             then Just (reverse path)
             else
               let ns = [ n | n <- neighbors s, not (Set.member n visited) ]
                   visited' = foldr Set.insert visited ns
                   q2 = foldl (\acc n -> acc |> (n:path)) q1 ns
               in go q2 visited'

-- Підрахунок кількості кроків (кількість станів мінус 1)
countSteps :: [a] -> Int
countSteps path = length path - 1

-- Виведення одного стану
printState :: Int -> State -> IO ()
printState k (State b l r) = do
  putStrLn $ "\nКрок " ++ show k ++ ":"
  putStrLn $ "  Човен: " ++ show b
  putStrLn $ "  Лівий берег:  " ++ show l
  putStrLn $ "  Правий берег: " ++ show r

-- Виведення всього розв’язку
printSolution :: [State] -> IO ()
printSolution path = do
  putStrLn "--- Розвʼязок: ---"
  mapM_ (uncurry printState) (zip [0..] path)
  putStrLn $ "\nЗагальна кількість кроків: " ++ show (countSteps path)

-- Точка входу
main :: IO ()
main =
  case bfs initialState of
    Nothing   -> putStrLn "Розв'язок не знайдено!"
    Just path -> printSolution path