module Arkham.Act.Cards.AscendingTheHillV3 (AscendingTheHillV3 (..), ascendingTheHillV3) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait

newtype AscendingTheHillV3 = AscendingTheHillV3 ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ascendingTheHillV3 :: ActCard AscendingTheHillV3
ascendingTheHillV3 = act (2, A) AscendingTheHillV3 Cards.ascendingTheHillV3 Nothing

instance HasModifiersFor AscendingTheHillV3 where
  getModifiersFor (AscendingTheHillV3 attrs) = do
    modifySelect attrs Anywhere [NonTraitRestrictedModifier Altered CannotPlaceClues]

instance HasAbilities AscendingTheHillV3 where
  getAbilities (AscendingTheHillV3 x) = [mkAbility x 1 $ forced $ Enters #when You "Sentinel Peak"]

instance RunMessage AscendingTheHillV3 where
  runMessage msg a@(AscendingTheHillV3 attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ AdvanceAct (toId attrs) (attrs.ability 1) #other
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      sentinelPeak <- selectJust $ LocationWithTitle "Sentinel Peak"
      sethBishop <- genCard Enemies.sethBishop
      createSethBishop <- createEnemyAt_ sethBishop sentinelPeak (Just $ toTarget attrs)
      pushAll [createSethBishop, advanceActDeck attrs]
      pure a
    CreatedEnemyAt eid _ target | isTarget attrs target -> do
      damage <- perPlayer 1
      push $ PlaceDamage (toSource attrs) (toTarget eid) damage
      pure a
    _ -> AscendingTheHillV3 <$> runMessage msg attrs
