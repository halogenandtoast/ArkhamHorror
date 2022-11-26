module Arkham.Location.Cards.CrumblingPrecipice
  ( crumblingPrecipice
  , CrumblingPrecipice(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype CrumblingPrecipice = CrumblingPrecipice LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crumblingPrecipice :: LocationCard CrumblingPrecipice
crumblingPrecipice = symbolLabel
  $ location CrumblingPrecipice Cards.crumblingPrecipice 4 (Static 0)

instance HasAbilities CrumblingPrecipice where
  getAbilities (CrumblingPrecipice a) = withBaseAbilities
    a
    [ restrictedAbility a 1 Here $ ForcedAbility $ AttemptExplore
        Timing.When
        You
    ]

instance RunMessage CrumblingPrecipice where
  runMessage msg l@(CrumblingPrecipice attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ BeginSkillTest
        iid
        (toSource attrs)
        (InvestigatorTarget iid)
        Nothing
        SkillWillpower
        4
      pure l
    FailedSkillTest iid _ (isSource attrs -> True) _ SkillWillpower _ -> do
      push $ BeginSkillTest
        iid
        (toSource attrs)
        (InvestigatorTarget iid)
        Nothing
        SkillAgility
        3
      pure l
    FailedSkillTest iid _ (isSource attrs -> True) _ SkillAgility _ -> do
      push $ BeginSkillTest
        iid
        (toSource attrs)
        (InvestigatorTarget iid)
        Nothing
        SkillCombat
        2
      pure l
    FailedSkillTest iid _ (isSource attrs -> True) _ SkillCombat _ -> do
      push $ InvestigatorKilled (toSource attrs) iid
      pure l
    _ -> CrumblingPrecipice <$> runMessage msg attrs
