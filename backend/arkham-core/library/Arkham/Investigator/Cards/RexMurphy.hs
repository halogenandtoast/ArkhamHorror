module Arkham.Investigator.Cards.RexMurphy (RexMurphy (..), rexMurphy) where

import Arkham.Ability
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection

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
        (freeReaction $ SuccessfulInvestigationResult #after You Anywhere (atLeast 2))
    ]

instance HasChaosTokenValue RexMurphy where
  getChaosTokenValue iid ElderSign (RexMurphy attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 2)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage RexMurphy where
  runMessage msg i@(RexMurphy attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ discoverAtYourLocation iid (toAbilitySource attrs 1) 1
      pure i
    ResolveChaosToken _drawnToken ElderSign iid | iid == toId attrs -> do
      drawing <- drawCards iid (ChaosTokenEffectSource ElderSign) 3
      player <- getPlayer iid
      push
        $ chooseOne player
        $ [ Label "Automatically fail to draw 3" [FailSkillTest, drawing]
          , Label "Resolve normally" []
          ]
      pure i
    _ -> RexMurphy <$> runMessage msg attrs
