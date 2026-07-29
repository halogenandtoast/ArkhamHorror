module Arkham.Investigator.Cards.IsabelleBarnes (isabelleBarnes) where

import Arkham.Ability
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Projection

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
            [MustBeCommitted, ShuffleIntoDeckInsteadOfDiscard]
          skillTestModifiers sid (attrs.ability 1) iid [AsIfInHandFor NotForPlay card.id]
          commitCard iid card
          push $ HandleTargetChoice iid (attrs.ability 1) (CardIdTarget card.id)
      pure i
    -- ShuffleIntoDeckInsteadOfDiscard only fires if the card is still committed
    -- when the test ends. If something (e.g. Butterfly Effect) pulls it out of
    -- the test first, we still have to shuffle it in. This has to be registered
    -- from the top level: insertAfterMatching only reaches one queue layer up,
    -- so it can't see EndSkillTestWindow from inside a chooseTargetM branch.
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (CardIdTarget cid) -> do
      afterSkillTestQuiet $ push $ DoStep 1 $ HandleTargetChoice iid (attrs.ability 1) (CardIdTarget cid)
      pure i
    DoStep 1 (HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (CardIdTarget cid)) -> do
      -- Only shuffle from hand or discard: the card is already in the deck on
      -- the normal path, and we must not resurrect one that was devoured,
      -- exiled, or otherwise removed.
      inHand <- fieldMap InvestigatorHand (any ((== cid) . toCardId)) iid
      inDiscard <- fieldMap InvestigatorDiscard (any ((== cid) . toCardId)) iid
      when (inHand || inDiscard) do
        card <- fetchCard cid
        push $ ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [card]
      pure i
    _ -> IsabelleBarnes <$> liftRunMessage msg attrs
