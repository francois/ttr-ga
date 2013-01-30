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

data City = Vancouver
          | Calgary
          | Portland
          | Seattle
      deriving (Show, Eq, Ord)

data Track = Track TrackColor Int
      deriving (Show, Eq)

data Route = Route City City [Track]
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

calgaryToVancouver = Route Calgary Vancouver [gray 4]
portlandToSeattle  = Route Portland Seattle [gray 2, gray 2]
seattleToVancouver = Route Seattle Vancouver [gray 2, gray 2]

pointsForTrack :: Track -> Int
pointsForTrack (Track _ 1) = 1
pointsForTrack (Track _ 2) = 2
pointsForTrack (Track _ 3) = 4
pointsForTrack (Track _ 4) = 7
pointsForTrack (Track _ 5) = 10
pointsForTrack (Track _ 6) = 15
pointsForTrack (Track _ 7) = 21
pointsForTrack (Track _ 8) = undefined

firstTrackOfRoute :: Route -> Track
firstTrackOfRoute (Route _ _ t) = head t

pointsForRoute :: [Route] -> Int
pointsForRoute rs = sum $ map pointsForTrack $ map firstTrackOfRoute rs
