module Arkham.Homebrew.AgainstTheWendigo.Treacheries.Campfire (campfire) where

import Arkham.Capability
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Helpers (Deck (..))
import Arkham.Helpers.Scenario (getEncounterDeck)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Helpers (scenarioI18n)
import Arkham.Homebrew.AgainstTheWendigo.Traits (pattern Animal)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype Campfire = Campfire TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

campfire :: TreacheryCard Campfire
campfire = treachery Campfire Cards.campfire

instance RunMessage Campfire where
  runMessage msg t@(Campfire attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      present <- select $ InvestigatorAt (locationWithInvestigator iid) <> can.heal.any (toSource attrs)
      scenarioI18n $ scope "campfire" $ chooseUpToNM iid (length present) "done" do
        targets present \other -> do
          healDamage other attrs 1
          healHorror other attrs 1
          doStep 1 msg
      doStep 2 msg
      pure t
    DoStep 1 (Revelation _iid (isSource attrs -> True)) -> do
      let n = toResultDefault @Int 0 attrs.meta
      pure $ Campfire $ attrs & setMeta (n + 1)
    DoStep 2 (Revelation iid (isSource attrs -> True)) -> do
      let n = toResultDefault @Int 0 attrs.meta
      when (n > 0) do
        deck <- getEncounterDeck
        let cards = take n (unDeck deck)
        focusCards cards do
          let nonAnimals = filterCards (CardWithType EnemyType <> not_ (CardWithTrait Animal)) cards
          if null nonAnimals
            then continue_ iid
            else chooseOneAtATimeM iid $ targets nonAnimals $ drawCard iid
        shuffleDeck Deck.EncounterDeck
      pure t
    _ -> Campfire <$> liftRunMessage msg attrs
