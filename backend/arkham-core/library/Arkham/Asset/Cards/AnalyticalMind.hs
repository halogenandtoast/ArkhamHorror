module Arkham.Asset.Cards.AnalyticalMind
  ( analyticalMind
  , AnalyticalMind(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Target
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
      [CanCommitToSkillTestPerformedByAnInvestigatorAtAnotherLocation 1]
  getModifiersFor _ _ = pure []

instance RunMessage AnalyticalMind where
  runMessage msg a@(AnalyticalMind attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (DrawCards iid 1 False)
    _ -> AnalyticalMind <$> runMessage msg attrs
