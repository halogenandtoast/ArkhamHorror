module Arkham.Treachery.Cards.RestlessJourneyFallacy (
  restlessJourneyFallacy,
  restlessJourneyFallacyEffect,
  RestlessJourneyFallacy (..),
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

newtype RestlessJourneyFallacy = RestlessJourneyFallacy TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

restlessJourneyFallacy :: TreacheryCard RestlessJourneyFallacy
restlessJourneyFallacy = treacheryWith RestlessJourneyFallacy Cards.restlessJourneyFallacy (setMeta @Bool False)

instance HasModifiersFor RestlessJourneyFallacy where
  getModifiersFor (InvestigatorTarget iid) (RestlessJourneyFallacy attrs) | treacheryInHandOf attrs == Just iid = do
    commitedCardsCount <- fieldMap InvestigatorCommittedCards length iid
    let alreadyCommitted = toResult @Bool attrs.meta
    modified attrs $ guard (alreadyCommitted || commitedCardsCount >= 1) *> [CannotCommitCards AnyCard]
  getModifiersFor _ _ = pure []

instance HasAbilities RestlessJourneyFallacy where
  getAbilities (RestlessJourneyFallacy a) = [restrictedAbility a 1 InYourHand $ FastAbility Free]

instance RunMessage RestlessJourneyFallacy where
  runMessage msg t@(RestlessJourneyFallacy attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeTreachery attrs (HiddenInHand iid)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      createCardEffect Cards.restlessJourneyFallacy Nothing (CardIdSource $ toCardId attrs) iid
      pure t
    EndRound -> pure $ RestlessJourneyFallacy $ setMeta @Bool False attrs
    SkillTestEnds {} -> do
      case attrs.placement of
        HiddenInHand iid -> do
          commitedCardsCount <- fieldMap InvestigatorCommittedCards length iid
          pure $ RestlessJourneyFallacy $ setMeta @Bool (commitedCardsCount > 0) attrs
        _ -> pure t
    _ -> RestlessJourneyFallacy <$> liftRunMessage msg attrs

newtype RestlessJourneyFallacyEffect = RestlessJourneyFallacyEffect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

restlessJourneyFallacyEffect :: EffectArgs -> RestlessJourneyFallacyEffect
restlessJourneyFallacyEffect = cardEffect RestlessJourneyFallacyEffect Cards.restlessJourneyFallacy

instance RunMessage RestlessJourneyFallacyEffect where
  runMessage msg e@(RestlessJourneyFallacyEffect attrs) = runQueueT $ case msg of
    CreatedEffect eid _ source (InvestigatorTarget iid) | eid == attrs.id -> do
      sid <- getRandom
      beginSkillTest sid iid source iid #intellect (Fixed 3)
      pure e
    SkillTestEnds _ _iid (isSource attrs -> True) -> disableReturn e
    FailedThisSkillTest _iid source | attrs.source == source -> do
      placeDoomOnAgendaAndCheckAdvance 1
      disableReturn e
    _ -> RestlessJourneyFallacyEffect <$> liftRunMessage msg attrs
