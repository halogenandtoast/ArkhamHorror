module Arkham.Treachery.Cards.DangerousCuriosity (dangerousCuriosity) where

import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Scenario (getEncounterDeckKey, getEncounterDiscard)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Trait (Trait (Stowaway))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DangerousCuriosity = DangerousCuriosity TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dangerousCuriosity :: TreacheryCard DangerousCuriosity
dangerousCuriosity = treachery DangerousCuriosity Cards.dangerousCuriosity

instance RunMessage DangerousCuriosity where
  runMessage msg t@(DangerousCuriosity attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower
        $ InvestigatorLocationMaybeFieldCalculation iid LocationShroud
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid attrs 1
      -- Snapshot the cards already in the encounter discard so we only shuffle
      -- back the locations discarded by this effect.
      key <- getEncounterDeckKey iid
      existing <- map toCardId <$> getEncounterDiscard key
      discardUntilFirst iid attrs Deck.EncounterDeck (basic $ #enemy <> withTrait Stowaway)
      pure $ DangerousCuriosity $ setMeta existing attrs
    RequestedEncounterCard (isSource attrs -> True) (Just iid) mcard -> do
      key <- getEncounterDeckKey iid
      let before = toResultDefault @[CardId] [] attrs.meta
      discardedLocations <-
        filter (\c -> toCardType c == LocationType && toCardId c `notElem` before)
          <$> getEncounterDiscard key
      unless (null discardedLocations)
        $ shuffleCardsIntoDeck Deck.EncounterDeck discardedLocations
      for_ mcard (drawCard iid)
      pure t
    _ -> DangerousCuriosity <$> liftRunMessage msg attrs
