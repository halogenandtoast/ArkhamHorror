module Arkham.Investigator.Cards.RexMurphy (rexMurphy) where

import Arkham.Ability
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Taboo

newtype RexMurphy = RexMurphy InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

rexMurphy :: InvestigatorCard RexMurphy
rexMurphy =
  investigator RexMurphy Cards.rexMurphy
    $ Stats {health = 6, sanity = 9, willpower = 3, intellect = 4, combat = 2, agility = 3}

instance HasAbilities RexMurphy where
  getAbilities (RexMurphy x) =
    [ (if maybe False (>= TabooList15) x.taboo then playerLimit PerRound else id)
        $ selfAbility x 1 (AbleToDiscoverCluesAt YourLocation)
        $ freeReaction (SuccessfulInvestigationResult #after You Anywhere (atLeast 2))
    ]

instance HasChaosTokenValue RexMurphy where
  getChaosTokenValue iid ElderSign (RexMurphy attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 2)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage RexMurphy where
  runMessage msg i@(RexMurphy attrs) = runQueueT case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discoverAtYourLocation NotInvestigate iid (attrs.ability 1) 1
      pure i
    ResolveChaosToken _drawnToken ElderSign iid | iid == toId attrs -> do
      chooseOneM iid do
        labeled "Automatically fail to draw 3" $ failSkillTest >> drawCards iid ElderSign 3
        labeled "Resolve normally" nothing
      pure i
    _ -> RexMurphy <$> liftRunMessage msg attrs
