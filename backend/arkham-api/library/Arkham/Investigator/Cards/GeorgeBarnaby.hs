module Arkham.Investigator.Cards.GeorgeBarnaby (georgeBarnaby) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Window (cardDiscarded)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Investigator.Projection ()
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype GeorgeBarnaby = GeorgeBarnaby InvestigatorAttrs
  deriving anyclass IsInvestigator
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

georgeBarnaby :: InvestigatorCard GeorgeBarnaby
georgeBarnaby =
  investigator GeorgeBarnaby Cards.georgeBarnaby
    $ Stats {health = 7, sanity = 7, willpower = 2, intellect = 4, combat = 3, agility = 3}

instance HasModifiersFor GeorgeBarnaby where
  getModifiersFor (GeorgeBarnaby attrs) = do
    modifySelf attrs $ MaxHandSize (min 5 $ length $ investigatorCardsUnderneath attrs)
      : map CanCommitToSkillTestsAsIfInHand (investigatorCardsUnderneath attrs)

instance HasAbilities GeorgeBarnaby where
  getAbilities (GeorgeBarnaby x) =
    [ playerLimit PerPhase
        $ restricted x 1 (Self <> NotSetup <> criteria)
        $ freeReaction (DiscardedFromHand #after You AnySource #any)
    ]
   where
    criteria = if length (investigatorCardsUnderneath x) < 5 then NoRestriction else Never

instance HasChaosTokenValue GeorgeBarnaby where
  getChaosTokenValue iid ElderSign (GeorgeBarnaby attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage GeorgeBarnaby where
  runMessage msg i@(GeorgeBarnaby attrs) = runQueueT $ case msg of
    ElderSignEffect iid | iid == attrs.id -> do
      drawCards iid ElderSign 1
      pure i
    UseCardAbility iid (isSource attrs -> True) 1 (cardDiscarded -> card) _ -> do
      placeUnderneath iid [card]
      drawCards iid (attrs.ability 1) 1
      pure i
    After (InvestigatorMulligan iid) | iid == attrs.id -> do
      hand <- iid.hand
      chooseSomeM iid "Done placing cards underneath George Barnaby" do
        targets hand \card -> placeUnderneath iid [card]
      pure i
    _ -> GeorgeBarnaby <$> liftRunMessage msg attrs
