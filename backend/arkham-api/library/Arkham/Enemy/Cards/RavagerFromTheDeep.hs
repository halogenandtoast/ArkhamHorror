module Arkham.Enemy.Cards.RavagerFromTheDeep (ravagerFromTheDeep, RavagerFromTheDeep (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Direction
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfMaybe)
import Arkham.Helpers.Scenario (getGrid)
import Arkham.Location.FloodLevel
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Scenarios.InTooDeep.Helpers

newtype RavagerFromTheDeep = RavagerFromTheDeep EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ravagerFromTheDeep :: EnemyCard RavagerFromTheDeep
ravagerFromTheDeep = enemy RavagerFromTheDeep Cards.ravagerFromTheDeep (2, Static 4, 1) (2, 1)

instance HasModifiersFor RavagerFromTheDeep where
  getModifiersFor (RavagerFromTheDeep a) = modifySelfMaybe a do
    lid <- MaybeT $ getLocationOf a.id
    getFloodLevel lid <&> \case
      FullyFlooded -> [EnemyFight 2, EnemyEvade 2]
      PartiallyFlooded -> [EnemyFight 1, EnemyEvade 1]
      Unflooded -> []

instance HasAbilities RavagerFromTheDeep where
  getAbilities (RavagerFromTheDeep a) = extend a [mkAbility a 1 $ forced $ EnemyEngaged #after You (be a)]

instance RunMessage RavagerFromTheDeep where
  runMessage msg e@(RavagerFromTheDeep attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #combat (Fixed 2)
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      void $ runMaybeT do
        lid <- MaybeT $ getLocationOf iid
        grid <- lift getGrid
        pos <- hoistMaybe $ findInGrid lid grid
        GridLocation _ west <- hoistMaybe $ viewGrid (updatePosition pos West) grid
        lift $ placeBarrier lid west
      pure e
    _ -> RavagerFromTheDeep <$> liftRunMessage msg attrs
