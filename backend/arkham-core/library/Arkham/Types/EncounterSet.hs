module Arkham.Types.EncounterSet
  ( EncounterSet(..)
  , gatherEncounterSet
  )
where

import Arkham.Types.Card
import Arkham.Types.Card.Id
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import Data.UUID.V4
import Safe (fromJustNote)

data EncounterSet = TheGathering | Rats | Ghouls | StrikingFear | ChillingCold | TheMidnightMasks | Nightgaunts | DarkCult | LockedDoors | CultOfUmordhoth

gatherEncounterSet :: MonadIO m => EncounterSet -> m [EncounterCard]
gatherEncounterSet =
  traverse
      (\cid ->
        fromJustNote
            ("missing card" <> show cid)
            (HashMap.lookup cid allEncounterCards)
          . CardId
          <$> liftIO nextRandom
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
  TheMidnightMasks -> replicate 3 "01135" <> replicate 2 "01136"
  DarkCult -> replicate 3 "01169" <> ["01170"] <> replicate 2 "01171"
  Nightgaunts -> replicate 2 "01172" <> replicate 2 "01173"
  LockedDoors -> replicate 2 "01174"
  CultOfUmordhoth -> ["01137", "01138", "01139", "01140", "01141"]
