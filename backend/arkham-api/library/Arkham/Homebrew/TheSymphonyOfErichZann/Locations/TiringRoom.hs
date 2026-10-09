module Arkham.Homebrew.TheSymphonyOfErichZann.Locations.TiringRoom (tiringRoom) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype TiringRoom = TiringRoom LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tiringRoom :: LocationCard TiringRoom
tiringRoom =
  locationWith TiringRoom Cards.tiringRoom 2 (PerPlayer 1)
    $ costToEnterUnrevealedL
    .~ GroupClueCost (PerPlayer 1) YourLocation

instance HasModifiersFor TiringRoom where
  getModifiersFor (TiringRoom a) = do
    -- "The door leading to this room is blocked. As an additional cost to move
    -- to Backstage Room, the investigators must spend 1 clue per investigator,
    -- as a group."
    modifySelfWhen a (not a.revealed) [AdditionalCostToEnter $ GroupClueCost (PerPlayer 1) Anywhere]

instance HasAbilities TiringRoom where
  getAbilities (TiringRoom a) =
    extend
      a
      [ -- "At the end of the investigator phase: Ready all enemies at this location."
        mkAbility a 1 $ forced $ PhaseEnds #when #investigation
      , -- "[action]: Heal 1 damage and 1 horror. (Limit once per game)"
        playerLimit PerGame $ restricted a 2 Here actionAbility
      ]

instance RunMessage TiringRoom where
  runMessage msg l@(TiringRoom attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach (enemyAt attrs.id) (push . Ready . toTarget)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      healDamage iid (attrs.ability 2) 1
      healHorror iid (attrs.ability 2) 1
      pure l
    _ -> TiringRoom <$> liftRunMessage msg attrs
