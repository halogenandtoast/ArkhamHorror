module Arkham.Treachery.Cards.LightlessShadow (
  lightlessShadow,
  LightlessShadow (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.ScenarioLogKey
import Arkham.SkillType
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype LightlessShadow = LightlessShadow TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lightlessShadow :: TreacheryCard LightlessShadow
lightlessShadow = treachery LightlessShadow Cards.lightlessShadow

instance RunMessage LightlessShadow where
  runMessage msg t@(LightlessShadow attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      push
        $ RevelationSkillTest
          iid
          source
          SkillAgility
          (SkillTestDifficulty $ SumCalculation [Fixed 1, ScenarioCount CurrentDepth])
      pure t
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ ->
      do
        push $ InvestigatorAssignDamage iid (toSource attrs) DamageAny 2 0
        pure t
    _ -> LightlessShadow <$> runMessage msg attrs
