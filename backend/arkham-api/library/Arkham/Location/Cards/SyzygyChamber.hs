module Arkham.Location.Cards.SyzygyChamber (syzygyChamber, SyzygyChamber (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.FloodLevel
import Arkham.Location.Helpers (connectsToAdjacent)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Trait (Trait (AncientOne))

newtype SyzygyChamber = SyzygyChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

syzygyChamber :: LocationCard SyzygyChamber
syzygyChamber =
  locationWith SyzygyChamber Cards.syzygyChamber 0 (Static 0)
    $ connectsToAdjacent
    . (floodLevelL ?~ FullyFlooded)

instance HasAbilities SyzygyChamber where
  getAbilities (SyzygyChamber a) =
    extendRevealed
      a
      [ groupLimit PerGame
          $ restricted a 1 Here
          $ actionAbilityWithCost
          $ PlaceKeyCost (toTarget a) RedKey
          <> GroupClueCost (PerPlayer 1) (be a)
      , restrictedAbility a 2 (exists $ InvestigatorAt (be a))
          $ forced
          $ FloodLevelIncreased #after Anywhere
      ]

instance RunMessage SyzygyChamber where
  runMessage msg l@(SyzygyChamber attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectEach (EnemyWithTrait AncientOne) $ automaticallyEvadeEnemy iid
      setThisFloodLevel attrs Unflooded
      gameModifier (attrs.ability 1) attrs CannotBeFlooded
      pure l
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      selectEach (InvestigatorAt (be attrs)) \iid' -> do
        assignHorror iid' (attrs.ability 2) 1
      pure l
    _ -> SyzygyChamber <$> liftRunMessage msg attrs
