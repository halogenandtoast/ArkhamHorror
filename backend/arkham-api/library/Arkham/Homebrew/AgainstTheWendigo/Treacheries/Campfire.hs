module Arkham.Homebrew.AgainstTheWendigo.Treacheries.Campfire (campfire) where

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
    {- | "You can heal 1 damage and 1 horror to any investigators present in your
    location. For each investigator healed by this effect, look at 1 card from
    the top of the encounter deck. Draw every non-Animal Enemy you see. Then
    shuffle the other cards back into the encounter deck." -}
    Revelation iid (isSource attrs -> True) -> do
      present <- select $ InvestigatorAt $ locationWithInvestigator iid
      scenarioI18n $ chooseUpToNM iid (length present) (ikey "campfire.done") do
        targets present \other -> do
          healDamage other attrs 1
          healHorror other attrs 1
          doStep 1 msg
      pure t
    -- One peek per investigator healed.
    DoStep 1 (Revelation iid (isSource attrs -> True)) -> do
      deck <- getEncounterDeck
      case take 1 (unDeck deck) of
        [card] | toCard card `cardMatch` (CardWithType EnemyType <> not_ (CardWithTrait Animal)) -> do
          drawCard iid card
        _ -> shuffleDeck Deck.EncounterDeck
      pure t
    _ -> Campfire <$> liftRunMessage msg attrs
