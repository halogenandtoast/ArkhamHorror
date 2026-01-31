module Arkham.Investigator.Cards.IsabelleBarnes (isabelleBarnes) where

import Arkham.Ability
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype IsabelleBarnes = IsabelleBarnes InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

isabelleBarnes :: InvestigatorCard IsabelleBarnes
isabelleBarnes =
  investigator IsabelleBarnes Cards.isabelleBarnes
    $ Stats {health = 5, sanity = 9, willpower = 4, intellect = 2, combat = 3, agility = 3}

instance HasAbilities IsabelleBarnes where
  getAbilities (IsabelleBarnes a) =
    [ playerLimit PerRound
        $ wantsSkillTest (YourSkillTest AnySkillTest)
        $ selfAbility a 1 (exists $ CommittableCard You $ InDiscardOf You <> basic #skill)
        $ freeTrigger (DirectHorrorCost (a.ability 1) You 1)
    ]

instance HasChaosTokenValue IsabelleBarnes where
  getChaosTokenValue iid ElderSign (IsabelleBarnes attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage IsabelleBarnes where
  runMessage msg i@(IsabelleBarnes attrs) = runQueueT $ case msg of
    PassedSkillTestWithToken iid ElderSign | iid == toId attrs -> do
      healHorror iid (ElderSignEffectSource iid) 1
      pure i
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cards <- select $ CommittableCard (InvestigatorWithId iid) $ inDiscardOf iid <> basic #skill
      withSkillTest \sid -> do
        chooseTargetM iid cards \card -> do
          skillTestModifiers
            sid
            (attrs.ability 1)
            card.id
            [MustBeCommitted, NoAdditionalCosts, ShuffleIntoDeckInsteadOfDiscard]
          skillTestModifiers sid (attrs.ability 1) iid [AsIfInHandFor NotForPlay card.id]
          commitCard iid card
      pure i
    _ -> IsabelleBarnes <$> liftRunMessage msg attrs
