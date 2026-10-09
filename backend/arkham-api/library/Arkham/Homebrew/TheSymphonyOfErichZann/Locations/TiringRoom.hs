module Arkham.Homebrew.TheSymphonyOfErichZann.Locations.TiringRoom (tiringRoom) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype TiringRoom = TiringRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tiringRoom :: LocationCard TiringRoom
tiringRoom =
  locationWith TiringRoom Cards.tiringRoom 2 (PerPlayer 1)
    $ costToEnterUnrevealedL
    .~ GroupClueCost (PerPlayer 1) Anywhere

instance HasAbilities TiringRoom where
  getAbilities (TiringRoom a) =
    extendRevealed
      a
      [ restricted a 1 (exists $ EnemyAt (be a) <> #exhausted) $ forced $ PhaseEnds #when #investigation
      , playerLimit PerGame $ restricted a 2 (Here <> youExist (can.heal.any (a.ability 2))) actionAbility
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
