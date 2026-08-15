module Arkham.Homebrew.DarkMatter.Treacheries.HauntingPast (hauntingPast) where

import Arkham.Card
import Arkham.EncounterCard.Source (EncounterCardSource (FromEncounterDeck))
import Arkham.Helpers (Deck (..))
import Arkham.Helpers.Scenario (getEncounterDeck)
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (getMemories)
import Arkham.Keyword (Keyword (Hidden))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted
import Arkham.Zone qualified as Zone

newtype HauntingPast = HauntingPast TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hauntingPast :: TreacheryCard HauntingPast
hauntingPast = treachery HauntingPast Cards.hauntingPast

instance RunMessage HauntingPast where
  runMessage msg t@(HauntingPast attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      memories <- getMemories iid
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed memories)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      Deck deck <- getEncounterDeck
      case break (`cardMatch` CardWithKeyword Hidden) deck of
        (_, []) -> do
          push $ FoundCards $ singletonMap Zone.FromDeck (map toCard deck)
          chooseOneM iid $ labeled "$label.noMatchesFound" $ push $ ClearFound Zone.FromDeck
        (seen, card : _) -> do
          push $ FoundCards $ singletonMap Zone.FromDeck (map toCard $ seen <> [card])
          chooseOneM iid
            $ targeting (toCardId card)
            $ push
            $ FoundAndDrewEncounterCard iid FromEncounterDeck card
      pure t
    _ -> HauntingPast <$> liftRunMessage msg attrs
