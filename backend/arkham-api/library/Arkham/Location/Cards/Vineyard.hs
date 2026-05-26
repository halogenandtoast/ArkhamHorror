module Arkham.Location.Cards.Vineyard (vineyard) where

import Arkham.Ability
import Arkham.Helpers.Window (enteringEnemy)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose

newtype Vineyard = Vineyard LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vineyard :: LocationCard Vineyard
vineyard = symbolLabel $ locationWith Vineyard Cards.vineyard 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities Vineyard where
  getAbilities (Vineyard a) =
    extendRevealed
      a
      [ playerLimit PerTurn
          $ restricted a 1 (Here <> DuringTurn You <> exists (enemyAt a))
          $ FastAbility Free
      , mkAbility a 2 $ forced $ EnemyEnters #when (be a) AnyEnemy
      ]

instance RunMessage Vineyard where
  runMessage msg l@(Vineyard attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ enemyAt attrs
      chooseTargetM iid enemies $ engageEnemy iid
      pure l
    UseCardAbility _ (isSource attrs -> True) 2 (enteringEnemy -> eid) _ -> do
      healDamage eid (attrs.ability 2) 1
      pure l
    _ -> Vineyard <$> liftRunMessage msg attrs
