module Arkham.Enemy.Cards.DimensionalShambler (
  dimensionalShambler,
  DimensionalShambler (..),
)
where

import Arkham.Prelude

import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.RequestedChaosTokenStrategy

newtype DimensionalShambler = DimensionalShambler EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimensionalShambler :: EnemyCard DimensionalShambler
dimensionalShambler = enemy DimensionalShambler Cards.dimensionalShambler (4, PerPlayer 4, 3) (2, 2)

instance HasAbilities DimensionalShambler where
  getAbilities (DimensionalShambler attrs) =
    withBaseAbilities
      attrs
      [mkAbility attrs 1 $ ForcedAbility $ InvestigatorTakeDamage #after You $ SourceIs $ toSource attrs]

instance RunMessage DimensionalShambler where
  runMessage msg e@(DimensionalShambler attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ RequestChaosTokens (toAbilitySource attrs 1) (Just iid) (Reveal 1) SetAside
      pure e
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) (map chaosTokenFace -> tokens) -> do
      push $ ResetChaosTokens (toAbilitySource attrs 1)
      when (#autofail `elem` tokens) $ do
        pushAll [RemoveEnemy (toId attrs), Msg.InvestigatorDefeated (toAbilitySource attrs 1) iid]
      pure e
    _ -> DimensionalShambler <$> runMessage msg attrs
