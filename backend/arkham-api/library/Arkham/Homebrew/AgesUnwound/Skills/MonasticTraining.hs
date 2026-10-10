module Arkham.Homebrew.AgesUnwound.Skills.MonasticTraining (monasticTraining) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.AgesUnwound.CardDefs.Skills qualified as Cards
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Skill.Import.Lifted hiding (RevealChaosToken)

newtype MonasticTraining = MonasticTraining SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | /Gratitude/ adds this to a hand mid-scenario; Scenario V's resolution lets a
player add it to a deck.

TODO(ages-unwound): the discard-pile ability needs @cdOutOfPlayEffects =
[InDiscardEffect]@ on the def, or @preloadDiscardEntities@ never builds the
entity and 'getAbilities' is never consulted while the card is in the discard
pile. @CardDefs/Skills.hs@ is the orchestrator's, so the ability below is
correct but dormant until that lands.
-}
monasticTraining :: SkillCard MonasticTraining
monasticTraining = skill MonasticTraining Cards.monasticTraining

{- | "While Monastic Training is in your discard pile, when you reveal a [skull]
token during a skill test, after this test resolves you may discard a card to add
Monastic Training to your hand."

The reaction is offered at the reveal, which is the trigger the card names; what
it schedules is the discard-and-return, which the card defers to after the test.
-}
instance HasAbilities MonasticTraining where
  getAbilities (MonasticTraining a) =
    [restricted a 1 InYourDiscard $ freeReaction $ RevealChaosToken #after You #skull]

instance RunMessage MonasticTraining where
  runMessage msg s@(MonasticTraining attrs) = runQueueT $ case msg of
    InDiscard _ (UseThisAbility iid (isSource attrs -> True) 1) -> do
      withSkillTest \sid -> afterThisTestResolves sid do
        cards <- select $ inHandOf NotForPlay iid <> basic AnyCard
        chooseOneM iid do
          withI18n skip_
          targets cards \card -> do
            push $ DiscardCard iid (attrs.ability 1) (toCardId card)
            addToHand iid (only attrs)
      pure s
    _ -> MonasticTraining <$> liftRunMessage msg attrs
