module Arkham.Treachery.Cards.RestlessJourneyHardship (
  restlessJourneyHardship,
  restlessJourneyHardshipEffect,
  RestlessJourneyHardship (..),
)
where

import Arkham.Ability
import Arkham.Card
import Arkham.Effect.Import
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher (CardMatcher (AnyCard))
import Arkham.Placement
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype RestlessJourneyHardship = RestlessJourneyHardship TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

restlessJourneyHardship :: TreacheryCard RestlessJourneyHardship
restlessJourneyHardship = treacheryWith RestlessJourneyHardship Cards.restlessJourneyHardship (setMeta @Bool False)

instance HasModifiersFor RestlessJourneyHardship where
  getModifiersFor (InvestigatorTarget iid) (RestlessJourneyHardship attrs) | treacheryInHandOf attrs == Just iid = do
    commitedCardsCount <- fieldMap InvestigatorCommittedCards length iid
    let alreadyCommitted = toResult @Bool attrs.meta
    modified attrs $ guard (alreadyCommitted || commitedCardsCount >= 1) *> [CannotCommitCards AnyCard]
  getModifiersFor _ _ = pure []

instance HasAbilities RestlessJourneyHardship where
  getAbilities (RestlessJourneyHardship a) =
    [restrictedAbility a 1 InYourHand $ FastAbility Free]

instance RunMessage RestlessJourneyHardship where
  runMessage msg t@(RestlessJourneyHardship attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeTreachery attrs (HiddenInHand iid)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      createCardEffect Cards.restlessJourneyHardship Nothing (CardIdSource $ toCardId attrs) iid
      pure t
    EndRound -> pure $ RestlessJourneyHardship $ setMeta @Bool False attrs
    SkillTestEnds {} -> do
      case attrs.placement of
        HiddenInHand iid -> do
          commitedCardsCount <- fieldMap InvestigatorCommittedCards length iid
          pure $ RestlessJourneyHardship $ setMeta @Bool (commitedCardsCount > 0) attrs
        _ -> pure t
    _ -> RestlessJourneyHardship <$> liftRunMessage msg attrs

newtype RestlessJourneyHardshipEffect = RestlessJourneyHardshipEffect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

restlessJourneyHardshipEffect :: EffectArgs -> RestlessJourneyHardshipEffect
restlessJourneyHardshipEffect = cardEffect RestlessJourneyHardshipEffect Cards.restlessJourneyHardship

instance RunMessage RestlessJourneyHardshipEffect where
  runMessage msg e@(RestlessJourneyHardshipEffect attrs) = runQueueT $ case msg of
    CreatedEffect eid _ source (InvestigatorTarget iid) | eid == attrs.id -> do
      beginSkillTest iid source iid #combat (Fixed 3)
      pure e
    SkillTestEnds _iid (isSource attrs -> True) -> disableReturn e
    FailedThisSkillTest _iid source | attrs.source == source -> do
      placeDoomOnAgendaAndCheckAdvance 1
      disableReturn e
    _ -> RestlessJourneyHardshipEffect <$> liftRunMessage msg attrs
