module Arkham.Asset.Cards.AnalyticalMind
  ( analyticalMind
  , AnalyticalMind(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype AnalyticalMind = AnalyticalMind AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

analyticalMind :: AssetCard AnalyticalMind
analyticalMind = asset AnalyticalMind Cards.analyticalMind

instance HasAbilities AnalyticalMind where
  getAbilities (AnalyticalMind attrs) =
    [ restrictedAbility attrs 1 ControlsThis
        $ ReactionAbility
            (CommittedCards Timing.After You $ LengthIs $ EqualTo $ Static 1)
        $ ExhaustCost (toTarget attrs)
    ]

instance HasModifiersFor AnalyticalMind where
  getModifiersFor (InvestigatorTarget iid) (AnalyticalMind attrs)
    | controlledBy attrs iid = pure $ toModifiers
      attrs
      [CanCommitToSkillTestPerformedByAnInvestigatorAt Anywhere]
  getModifiersFor _ _ = pure []

instance RunMessage AnalyticalMind where
  runMessage msg a@(AnalyticalMind attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      drawing <- drawCards iid attrs 1
      push drawing
      pure a
    _ -> AnalyticalMind <$> runMessage msg attrs
