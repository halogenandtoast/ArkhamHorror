module Arkham.Treachery.Cards.DeadlyFate (deadlyFate) where

import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.APhantomOfTruth.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DeadlyFate = DeadlyFate TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deadlyFate :: TreacheryCard DeadlyFate
deadlyFate = treachery DeadlyFate Cards.deadlyFate

instance RunMessage DeadlyFate where
  runMessage msg t@(DeadlyFate attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      discardUntilFirst iid attrs Deck.EncounterDeck $ basic #enemy
      pure t
    RequestedEncounterCard (isSource attrs -> True) (Just iid) mcard -> do
      case mcard of
        Nothing -> assignHorror iid attrs 1
        Just c -> do
          -- tricky, we must create an enemy that has been discaded, have it
          -- attack,  and then remove it
          -- This technically means we have an enemy at no location
          focusCard c do
            chooseOneM iid $ scenarioI18n do
              labeled' "deadlyFate.drawEnemy" $ drawCard iid c
              labeled' "deadlyFate.attack " do
                addToEncounterDiscard (only c)
                push $ EnemyAttackFromDiscard iid (toSource attrs) (EncounterCard c)
      pure t
    _ -> DeadlyFate <$> liftRunMessage msg attrs
