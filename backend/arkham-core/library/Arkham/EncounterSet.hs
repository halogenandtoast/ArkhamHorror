module Arkham.EncounterSet (module X, gatherEncounterSet) where

import Arkham.Prelude

import Arkham.EncounterCard
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.EncounterCard
import Arkham.Types.EncounterSet as X

gatherEncounterSet :: MonadRandom m => EncounterSet -> m [EncounterCard]
gatherEncounterSet = traverse genEncounterCard . setCards

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
  ExtracurricularActivity -> [] -- all cards are set aside
  TheHouseAlwaysWins -> replicate 2 "02081" <> replicate 2 "02082"
  TheMiskatonicMuseum ->
    ["02141", "02142"]
      <> replicate 2 "02143"
      <> replicate 3 "02144"
      <> replicate 2 "02145"
      <> replicate 2 "02146"
  TheEssexCountyExpress ->
    replicate 3 "02179"
      <> replicate 3 "02180"
      <> replicate 3 "02181"
      <> replicate 2 "02182"
      <> replicate 2 "02183"
  BloodOnTheAltar ->
    "02216"
      : replicate 3 "02220"
      <> replicate 2 "02221"
      <> replicate 2 "02222"
      <> replicate 3 "02223"
      <> replicate 3 "02224"
  UndimensionedAndUnseen ->
    replicate 5 "02255"
      <> replicate 4 "02256"
      <> replicate 3 "02257"
      <> replicate 2 "02258"
      <> replicate 2 "02259"
  WhereDoomAwaits ->
    "02293"
      : replicate 2 "02294"
      <> ["02295"]
      <> replicate 3 "02296"
      <> replicate 3 "02297"
  LostInTimeAndSpace ->
    "02323"
      : replicate 4 "02324"
      <> replicate 2 "02325"
      <> replicate 2 "02326"
      <> ["02327", "02328"]
      <> replicate 3 "02329"
      <> replicate 2 "02330"
      <> replicate 3 "02331"
      <> replicate 2 "02332"
      <> replicate 3 "02333"
      <> replicate 3 "02298"
  Sorcery -> replicate 3 "02083" <> replicate 3 "02084"
  BishopsThralls -> replicate 2 "02085" <> replicate 3 "02086" <> ["02087"]
  Dunwich -> replicate 2 "02088" <> replicate 2 "02089"
  Whippoorwills -> replicate 3 "02090" <> replicate 2 "02091"
  BadLuck -> replicate 3 "02092" <> replicate 3 "02093"
  BeastThralls ->
    replicate 2 "02094" <> replicate 2 "02095" <> replicate 2 "02096"
  NaomisCrew ->
    replicate 2 "02097" <> replicate 2 "02098" <> replicate 2 "02099"
  TheBeyond ->
    replicate 2 "02100" <> replicate 2 "02101" <> replicate 2 "02102"
  HideousAbominations -> replicate 2 "02103" <> ["02104"]
  ReturnToTheGathering -> ["50022", "50023"] <> replicate 2 "50024"
  ReturnToTheMidnightMasks -> replicate 2 "50031"
  ReturnCultOfUmordhoth -> ["50044", "50045", "50046"]
  ReturnToTheDevourerBelow -> replicate 2 "50037"
  GhoulsOfUmordhoth -> replicate 3 "50038" <> ["50039"] <> replicate 3 "50040"
  TheDevourersCult -> replicate 3 "50041" <> ["50042"] <> replicate 2 "50043"
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
  Test -> []
