{-# LANGUAGE TemplateHaskell #-}

{- | Trait values owned by The Symphony of Erich Zann.

@Music@ is the scenario's load-bearing trait: a @Music@ treachery is put into
play next to the agenda deck instead of being discarded, and the agenda caps how
many may sit there at once. The four instrument traits (@Brass@, @String@,
@Percussion@, @Piano@) each gate one @Musician@ enemy -- that enemy can only be
parleyed with or damaged while a treachery of its instrument is in play.

@AuseilTheatre@ and @Backstage@ divide the map: the Backstage Rooms are only
unlocked when act 2 advances.
-}
module Arkham.Homebrew.TheSymphonyOfErichZann.Traits (module Arkham.Homebrew.TheSymphonyOfErichZann.Traits) where

import Arkham.Homebrew.TH (declareHomebrewTraits)

declareHomebrewTraits
  [ "AuseilTheatre"
  , "Backstage"
  , "Brass"
  , "Music"
  , "Musician"
  , "Percussion"
  , "Piano"
  , "String"
  ]
