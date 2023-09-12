module Arkham.Asset.Cards.AnalyticalMind (
  analyticalMind,
  AnalyticalMind (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype AnalyticalMind = AnalyticalMind AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

analyticalMind :: AssetCard AnalyticalMind
analyticalMind = asset AnalyticalMind Cards.analyticalMind

instance HasAbilities AnalyticalMind where
  getAbilities (AnalyticalMind attrs) =
    [ restrictedAbility attrs 1 ControlsThis
        $ ReactionAbility (CommittedCards Timing.After You $ LengthIs $ EqualTo $ Static 1)
        $ exhaust attrs
    ]

instance HasModifiersFor AnalyticalMind where
  getModifiersFor (InvestigatorTarget iid) (AnalyticalMind attrs) | controlledBy attrs iid = do
    pure $ toModifiers attrs [CanCommitToSkillTestPerformedByAnInvestigatorAt Anywhere]
  getModifiersFor _ _ = pure []

instance RunMessage AnalyticalMind where
  runMessage msg a@(AnalyticalMind attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushM $ drawCards iid (toAbilitySource attrs 1) 1
      pure a
    _ -> AnalyticalMind <$> runMessage msg attrs
