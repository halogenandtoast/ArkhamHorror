module Arkham.Types.EncounterSet
  ( EncounterSet(..)
  , gatherEncounterSet
  )
where

import Arkham.Types.Card
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import Safe (fromJustNote)

data EncounterSet = TheGathering | Rats | Ghouls | StrikingFear | ChillingCold

gatherEncounterSet :: EncounterSet -> [EncounterCard]
gatherEncounterSet =
  map
      (\cid -> fromJustNote ("missing card" <> show cid)
        $ HashMap.lookup cid allEncounterCards
      )
    . setCards

setCards :: EncounterSet -> [CardCode]
setCards = \case
  TheGathering -> ["01118", "01119"]
  Rats -> replicate 3 "01159"
  Ghouls -> replicate 3 "01160" <> ["01161"] <> replicate 3 "01162"
  StrikingFear ->
    replicate 3 "01163" <> replicate 2 "01164" <> replicate 2 "01165"
  ChillingCold -> replicate 2 "01167" <> replicate 2 "01168"
