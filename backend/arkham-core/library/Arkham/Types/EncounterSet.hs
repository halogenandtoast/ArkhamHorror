module Arkham.Types.EncounterSet
  ( EncounterSet(..)
  , gatherEncounterSet
  )
where

import Arkham.Types.Card
import Arkham.Types.Card.Id
import ClassyPrelude
import Data.UUID.V4
import Safe (fromJustNote)

data EncounterSet
  = TheGathering
  | TheMidnightMasks
  | TheDevourerBelow
  | CultOfUmordhoth
  | Rats
  | Ghouls
  | StrikingFear
  | AncientEvils
  | ChillingCold
  | Nightgaunts
  | DarkCult
  | LockedDoors
  | AgentsOfHastur
  | AgentsOfYogSothoth
  | AgentsOfShubNiggurath
  | AgentsOfCthulhu
  | ReturnToTheGathering
  | GhoulsOfUmordhoth
  | TheBayou
  | CurseOfTheRougarou

gatherEncounterSet :: MonadIO m => EncounterSet -> m [EncounterCard]
gatherEncounterSet =
  traverse
      (\cid ->
        fromJustNote ("missing card" <> show cid) (lookup cid allEncounterCards)
          . CardId
          <$> liftIO nextRandom
      )
    . setCards

setCards :: EncounterSet -> [CardCode]
setCards = \case
  TheGathering -> ["01118", "01119"]
  TheMidnightMasks -> replicate 3 "01135" <> replicate 2 "01136"
  CultOfUmordhoth -> ["01137", "01138", "01139", "01140", "01141"]
  TheDevourerBelow -> replicate 2 "01158"
  Rats -> replicate 3 "01159"
  Ghouls -> replicate 3 "01160" <> ["01161"] <> replicate 3 "01162"
  StrikingFear ->
    replicate 3 "01163" <> replicate 2 "01164" <> replicate 2 "01165"
  AncientEvils -> replicate 3 "01166"
  ChillingCold -> replicate 2 "01167" <> replicate 2 "01168"
  DarkCult -> replicate 3 "01169" <> ["01170"] <> replicate 2 "01171"
  Nightgaunts -> replicate 2 "01172" <> replicate 2 "01173"
  LockedDoors -> replicate 2 "01174"
  AgentsOfHastur -> replicate 2 "01175" <> replicate 2 "01176"
  AgentsOfYogSothoth -> replicate 2 "01177" <> replicate 2 "01178"
  AgentsOfShubNiggurath -> "01179" : replicate 3 "01180"
  AgentsOfCthulhu -> replicate 2 "01181" <> replicate 2 "01182"
  ReturnToTheGathering -> ["50022", "50023"] <> replicate 2 "50024"
  GhoulsOfUmordhoth -> replicate 3 "50038" <> ["50039"] <> replicate 3 "50040"
  TheBayou ->
    replicate 2 "81022"
      <> replicate 3 "81023"
      <> replicate 3 "81024"
      <> replicate 3 "81025"
      <> replicate 4 "81026"
      <> replicate 3 "81027"
  CurseOfTheRougarou ->
    replicate 2 "81031"
      <> replicate 2 "81032"
      <> ["81033"]
      <> replicate 5 "81034"
      <> replicate 2 "81035"
      <> replicate 3 "81036"
