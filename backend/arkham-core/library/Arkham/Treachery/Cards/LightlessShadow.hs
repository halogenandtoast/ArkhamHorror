module Arkham.Treachery.Cards.LightlessShadow (lightlessShadow, LightlessShadow (..)) where

import Arkham.Classes
import Arkham.Prelude
import Arkham.ScenarioLogKey
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
      sid <- getRandom
      push
        $ revelationSkillTest sid iid source #agility
        $ SumCalculation [Fixed 1, ScenarioCount CurrentDepth]
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      push $ assignDamage iid attrs 2
      pure t
    _ -> LightlessShadow <$> runMessage msg attrs
