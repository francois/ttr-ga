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

data Segment = Segment City City [Track]
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

calgaryToVancouver = Segment Calgary Vancouver [gray 4]
portlandToSeattle  = Segment Portland Seattle [gray 2, gray 2]
seattleToVancouver = Segment Seattle Vancouver [gray 2, gray 2]
