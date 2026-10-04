module Arkham.Homebrew.TheSymphonyOfErichZann.Locations.AnechoicChamber (anechoicChamber) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelfWhen)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Locations qualified as Cards
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Import.Lifted
import Arkham.Cost
import Arkham.Matcher

newtype AnechoicChamber = AnechoicChamber LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

anechoicChamber :: LocationCard AnechoicChamber
anechoicChamber = location AnechoicChamber Cards.anechoicChamber 3 (PerPlayer 1)

instance HasModifiersFor AnechoicChamber where
  -- "Enemies at Anechoic Chamber lose aloof and do not perform attacks of opportunity."
  getModifiersFor (AnechoicChamber a) = do
    modifySelect a (enemyAt a.id) [RemoveKeyword Keyword.Aloof, CannotMakeAttacksOfOpportunity]
    -- "The door leading to this room is blocked. As an additional cost to move
    -- to Backstage Room, the investigators must spend 1 clue per investigator,
    -- as a group."
    modifySelfWhen a (not a.revealed) [AdditionalCostToEnter $ GroupClueCost (PerPlayer 1) Anywhere]

instance RunMessage AnechoicChamber where
  runMessage msg (AnechoicChamber attrs) = runQueueT $ AnechoicChamber <$> liftRunMessage msg attrs
