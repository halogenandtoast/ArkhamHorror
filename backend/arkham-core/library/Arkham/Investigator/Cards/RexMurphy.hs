module Arkham.Investigator.Cards.RexMurphy (
  RexMurphy (..),
  rexMurphy,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Projection
import Arkham.Timing qualified as Timing

newtype RexMurphy = RexMurphy InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rexMurphy :: InvestigatorCard RexMurphy
rexMurphy =
  investigator RexMurphy Cards.rexMurphy
    $ Stats {health = 6, sanity = 9, willpower = 3, intellect = 4, combat = 2, agility = 3}

instance HasAbilities RexMurphy where
  getAbilities (RexMurphy x) =
    [ (restrictedAbility x 1)
        (OnLocation LocationWithAnyClues <> CanDiscoverCluesAt YourLocation)
        (FreeReactionAbility $ SuccessfulInvestigationResult Timing.After You Anywhere (atLeast 2))
    ]

instance HasChaosTokenValue RexMurphy where
  getChaosTokenValue iid ElderSign (RexMurphy attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 2)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage RexMurphy where
  runMessage msg i@(RexMurphy attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      location <- fieldJust InvestigatorLocation iid
      push $ InvestigatorDiscoverClues iid location (toAbilitySource attrs 1) 1 Nothing
      pure i
    ResolveChaosToken _drawnToken ElderSign iid | iid == toId attrs -> do
      drawing <- drawCards iid (ChaosTokenEffectSource ElderSign) 3
      push
        $ chooseOne iid
        $ [ Label "Automatically fail to draw 3" [FailSkillTest, drawing]
          , Label "Resolve normally" []
          ]
      pure i
    _ -> RexMurphy <$> runMessage msg attrs
