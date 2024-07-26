module Arkham.Treachery.Cards.RestlessJourneyLies (
  restlessJourneyLies,
  restlessJourneyLiesEffect,
  RestlessJourneyLies (..),
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

newtype RestlessJourneyLies = RestlessJourneyLies TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

restlessJourneyLies :: TreacheryCard RestlessJourneyLies
restlessJourneyLies = treacheryWith RestlessJourneyLies Cards.restlessJourneyLies (setMeta @Bool False)

instance HasModifiersFor RestlessJourneyLies where
  getModifiersFor (InvestigatorTarget iid) (RestlessJourneyLies attrs) | treacheryInHandOf attrs == Just iid = do
    commitedCardsCount <- fieldMap InvestigatorCommittedCards length iid
    let alreadyCommitted = toResult @Bool attrs.meta
    modified attrs $ guard (alreadyCommitted || commitedCardsCount >= 1) *> [CannotCommitCards AnyCard]
  getModifiersFor _ _ = pure []

instance HasAbilities RestlessJourneyLies where
  getAbilities (RestlessJourneyLies a) = [skillTestAbility $ restrictedAbility a 1 InYourHand $ FastAbility Free]

instance RunMessage RestlessJourneyLies where
  runMessage msg t@(RestlessJourneyLies attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeTreachery attrs (HiddenInHand iid)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      createCardEffect Cards.restlessJourneyLies Nothing (CardIdSource $ toCardId attrs) iid
      pure t
    EndRound -> pure $ RestlessJourneyLies $ setMeta @Bool False attrs
    SkillTestEnds {} -> do
      case attrs.placement of
        HiddenInHand iid -> do
          commitedCardsCount <- fieldMap InvestigatorCommittedCards length iid
          pure $ RestlessJourneyLies $ setMeta @Bool (commitedCardsCount > 0) attrs
        _ -> pure t
    _ -> RestlessJourneyLies <$> liftRunMessage msg attrs

newtype RestlessJourneyLiesEffect = RestlessJourneyLiesEffect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

restlessJourneyLiesEffect :: EffectArgs -> RestlessJourneyLiesEffect
restlessJourneyLiesEffect = cardEffect RestlessJourneyLiesEffect Cards.restlessJourneyLies

instance RunMessage RestlessJourneyLiesEffect where
  runMessage msg e@(RestlessJourneyLiesEffect attrs) = runQueueT $ case msg of
    CreatedEffect eid _ source (InvestigatorTarget iid) | eid == attrs.id -> do
      sid <- getRandom
      beginSkillTest sid iid source iid #agility (Fixed 3)
      pure e
    SkillTestEnds _ _iid (isSource attrs -> True) -> disableReturn e
    FailedThisSkillTest _iid source | attrs.source == source -> do
      placeDoomOnAgendaAndCheckAdvance 1
      disableReturn e
    _ -> RestlessJourneyLiesEffect <$> liftRunMessage msg attrs
