module Arkham.Story.Cards.WaresOfBaharna (waresOfBaharna) where

import Arkham.Capability
import Arkham.Card
import Arkham.Helpers.Query (getLead)
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Projection
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Trait (Trait (Item, Supply))

newtype WaresOfBaharna = WaresOfBaharna StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

waresOfBaharna :: StoryCard WaresOfBaharna
waresOfBaharna = story WaresOfBaharna Cards.waresOfBaharna

instance RunMessage WaresOfBaharna where
  runMessage msg s@(WaresOfBaharna attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      investigators <-
        select
          $ at_ (locationIs Locations.baharna)
          <> DiscardWith (HasCard $ hasAnyTrait [Item, Supply])
          <> can.have.cards.leaveDiscard

      lead <- getLead
      chooseOrRunOneAtATimeM lead do
        questionLabeled "Choose investigator to decide on returning discard"
        targets investigators (handleTarget lead attrs)

      remember ObtainedSuppliesFromBaharna
      pure s
    HandleTargetChoice _ (isSource attrs -> True) (InvestigatorTarget iid) -> do
      discards <- fieldMap InvestigatorDiscard (map toCard) iid
      let cards = filter (`cardMatch` hasAnyTrait @CardMatcher [Item, Supply]) discards
      focusCards discards do
        chooseOneM iid do
          labeled "Do not return card" nothing
          targets cards (addToHand iid . only)

      pure s
    _ -> WaresOfBaharna <$> liftRunMessage msg attrs
