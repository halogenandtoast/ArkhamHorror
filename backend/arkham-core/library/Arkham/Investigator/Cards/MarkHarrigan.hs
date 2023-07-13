module Arkham.Investigator.Cards.MarkHarrigan
  ( markHarrigan
  , MarkHarrigan(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype MarkHarrigan = MarkHarrigan InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

markHarrigan :: InvestigatorCard MarkHarrigan
markHarrigan = investigatorWith
  MarkHarrigan
  Cards.markHarrigan
  Stats
    { willpower = 3
    , intellect = 2
    , combat = 5
    , agility = 3
    , health = 9
    , sanity = 5
    }
  (startsWithL .~ [Assets.sophieInLovingMemory])

instance HasAbilities MarkHarrigan where
  getAbilities (MarkHarrigan attrs) =
    [ restrictedAbility
          attrs
          1
          Self
          (ReactionAbility
            (OrWindowMatcher
              [ DealtDamage Timing.When AnySource You
              , AssetDealtDamage Timing.When AnySource (AssetControlledBy You)
              ]
            )
            Free
          )
        & (abilityLimitL .~ PlayerLimit PerPhase 1)
    ]

instance HasChaosTokenValue MarkHarrigan where
  getChaosTokenValue iid ElderSign (MarkHarrigan attrs) | iid == toId attrs = do
    let tokenValue' = PositiveModifier $ investigatorHealthDamage attrs
    pure $ ChaosTokenValue ElderSign tokenValue'
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage MarkHarrigan where
  runMessage msg i@(MarkHarrigan attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      drawing <- drawCards iid attrs 1
      push drawing
      pure i
    _ -> MarkHarrigan <$> runMessage msg attrs
