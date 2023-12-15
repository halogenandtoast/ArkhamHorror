module Arkham.Act.Cards.AscendingTheHillV2 (AscendingTheHillV2 (..), ascendingTheHillV2) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait

newtype AscendingTheHillV2 = AscendingTheHillV2 ActAttrs
  deriving anyclass (IsAct)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ascendingTheHillV2 :: ActCard AscendingTheHillV2
ascendingTheHillV2 = act (2, A) AscendingTheHillV2 Cards.ascendingTheHillV2 Nothing

instance HasModifiersFor AscendingTheHillV2 where
  getModifiersFor (LocationTarget _) (AscendingTheHillV2 attrs) =
    pure $ toModifiers attrs [TraitRestrictedModifier Altered CannotPlaceClues]
  getModifiersFor _ _ = pure []

instance HasAbilities AscendingTheHillV2 where
  getAbilities (AscendingTheHillV2 x) = [mkAbility x 1 $ forced $ Enters #when You "Sentinel Peak"]

instance RunMessage AscendingTheHillV2 where
  runMessage msg a@(AscendingTheHillV2 attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ AdvanceAct (toId attrs) (attrs.ability 1) #other
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      sentinelPeak <- selectJust $ LocationWithTitle "Sentinel Peak"
      sethBishop <- genCard Enemies.sethBishop
      createSethBishop <- createEnemyAt_ sethBishop sentinelPeak Nothing
      pushAll [createSethBishop, advanceActDeck attrs]
      pure a
    _ -> AscendingTheHillV2 <$> runMessage msg attrs
