module Arkham.Types.Investigator.Cards.MarkHarrigan
  ( markHarrigan
  , MarkHarrigan(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Assets
import Arkham.Types.Ability
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Timing qualified as Timing

newtype MarkHarrigan = MarkHarrigan InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor env)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

markHarrigan :: MarkHarrigan
markHarrigan =
  MarkHarrigan $ base & startsWithL .~ [Assets.sophieInLovingMemory]
 where
  base = baseAttrs
    "03001"
    "Mark Harrigan"
    Guardian
    Stats
      { willpower = 3
      , intellect = 2
      , combat = 5
      , agility = 3
      , health = 9
      , sanity = 5
      }
    [Veteran]

instance HasAbilities MarkHarrigan where
  getAbilities (MarkHarrigan attrs) =
    [ restrictedAbility
          attrs
          1
          Self
          (ReactionAbility
            (OrWindowMatcher
              [ DealtDamage Timing.When You
              , AssetDealtDamage Timing.When (AssetOwnedBy You)
              ]
            )
            Free
          )
        & (abilityLimitL .~ PlayerLimit PerPhase 1)
    ]

instance HasTokenValue env MarkHarrigan where
  getTokenValue (MarkHarrigan attrs) iid ElderSign | iid == toId attrs = do
    let tokenValue' = PositiveModifier $ investigatorHealthDamage attrs
    pure $ TokenValue ElderSign tokenValue'
  getTokenValue _ _ token = pure $ TokenValue token mempty

instance (InvestigatorRunner env) => RunMessage env MarkHarrigan where
  runMessage msg i@(MarkHarrigan attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      i <$ push (DrawCards iid 1 False)
    _ -> MarkHarrigan <$> runMessage msg attrs
