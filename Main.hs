module Main where

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

allCities = [Calgary, Portland, Seattle, Vancouver]

isLink :: City -> City -> Route -> Bool
isLink a1 b1 (Route a2 b2 _) = a1 == a2 && b1 == b2

directLinkBetween :: City -> City -> [[Route]]
directLinkBetween a b = [[ r | r <- allRoutes, isLink a b r ]]

routesBetween :: City -> City -> [[Route]]
routesBetween a b = [ [r0, r1] | r0 @ (Route c0 c1 _) <- allRoutes, r1 @ (Route c2 c3 _) <- allRoutes, c1 == c2, a == c0, b == c3]

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
