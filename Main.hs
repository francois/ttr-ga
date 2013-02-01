module Main where

import Data.List (nub, sort)

data TrackColor = TRed
                | TYellow
                | TBrown
                | TBlack
                | TGray
                | TGreen
                | TPink
                | TBlue
      deriving (Show, Eq)

data CardColor =  CLocomotive
                | CRed
                | CYellow
                | CBrown
                | CBlack
                | CGreen
                | CPink
                | CBlue
      deriving (Show, Eq)

data City = Calgary
          | Portland
          | Seattle
          | Vancouver
      deriving (Show, Eq, Ord)

data Track = Track TrackColor Int
      deriving (Show, Eq)

data Route = Route City City Int
      deriving (Show, Eq)

data Destination = Destination City City

red    = Track TRed
yellow = Track TYellow
brown  = Track TBrown
black  = Track TBlack
gray   = Track TGray
green  = Track TGreen
pink   = Track TPink
blue   = Track TBlue

calgaryToVancouver = Route Calgary Vancouver 3
calgaryToSeattle   = Route Calgary Seattle 4
portlandToSeattle  = Route Portland Seattle 1
seattleToVancouver = Route Seattle Vancouver 1

allRoutes = [ calgaryToVancouver,
              calgaryToSeattle,
              portlandToSeattle,
              seattleToVancouver]

allCities :: [City]
allCities = sort $ nub $ concat [ [a, b] | (Route a b _) <- allRoutes ]

connect :: (City, City) -> [Route]
connect (a, b) = undefined

cookie :: City -> City -> [Route] -> [Route]
cookie start dest [] = []
cookie start dest (r:rs)
    | start == startOfRoute r && dest == endOfRoute r = [r]
    | dest == endOfRoute r = [r]
    | start == startOfRoute r = r : cookie (endOfRoute r) dest rs

startOfRoute :: Route -> City
startOfRoute (Route a _ _) = a

endOfRoute :: Route -> City
endOfRoute (Route _ b _) = b

routesStartingAt :: City -> [Route]
routesStartingAt a = nub $ (++) (filter (\(Route c0 _ _) -> c0 == a) allRoutes) (filter (\(Route _ c1 _) -> c1 == a) allRoutes)

allConnections :: [[Route]]
allConnections = map connect [ (a, b) | a <- allCities, b <- allCities, a < b ]

isLink :: City -> City -> Route -> Bool
isLink a1 b1 (Route a2 b2 _) = a1 == a2 && b1 == b2

directLinkBetween :: City -> City -> [[Route]]
directLinkBetween a b = [[ r | r <- allRoutes, isLink a b r ]]

routesBetween :: City -> City -> [[Route]]
routesBetween a b = undefined

pointsForTrack :: Int -> Int
pointsForTrack 1 = 1
pointsForTrack 2 = 2
pointsForTrack 3 = 4
pointsForTrack 4 = 7
pointsForTrack 5 = 10
pointsForTrack 6 = 15
pointsForTrack 7 = 21
pointsForTrack 8 = 27

routeLength :: Route -> Int
routeLength (Route _ _ t) = t

pointsForRoute :: [Route] -> Int
pointsForRoute rs = sum $ map pointsForTrack $ map routeLength rs

main = do
  print $ cookie Calgary Seattle allRoutes
