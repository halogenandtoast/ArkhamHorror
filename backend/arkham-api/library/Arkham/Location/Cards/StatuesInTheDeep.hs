module Arkham.Location.Cards.StatuesInTheDeep (statuesInTheDeep, StatuesInTheDeep (..)) where

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

newtype StatuesInTheDeep = StatuesInTheDeep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

statuesInTheDeep :: LocationCard StatuesInTheDeep
statuesInTheDeep =
  locationWith StatuesInTheDeep Cards.statuesInTheDeep 0 (Static 0)
    $ connectsToAdjacent
    . (floodLevelL ?~ FullyFlooded)

instance HasAbilities StatuesInTheDeep where
  getAbilities (StatuesInTheDeep a) =
    extendRevealed
      a
      [ groupLimit PerGame
          $ restricted a 1 Here
          $ actionAbilityWithCost
          $ PlaceKeyCost (toTarget a) BlueKey
          <> GroupClueCost (PerPlayer 1) (be a)
      , restricted a 2 Here
          $ forced
          $ SkillTestResult #after You (WhileInvestigating $ be a) #failure
      ]

instance RunMessage StatuesInTheDeep where
  runMessage msg l@(StatuesInTheDeep attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectEach (EnemyWithTrait AncientOne) $ automaticallyEvadeEnemy iid
      setThisFloodLevel attrs Unflooded
      gameModifier (attrs.ability 1) attrs CannotBeFlooded
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      assignDamage iid (attrs.ability 2) 1
      pure l
    _ -> StatuesInTheDeep <$> liftRunMessage msg attrs
