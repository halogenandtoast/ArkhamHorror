module Arkham.Treachery.Cards.InevitableEnd (inevitableEnd) where

import Arkham.Card
import Arkham.Investigator.Projection ()
import Arkham.Matcher (pattern WeaknessCard)
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.WarOfTheOuterGods.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype InevitableEnd = InevitableEnd TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inevitableEnd :: TreacheryCard InevitableEnd
inevitableEnd = treachery InevitableEnd Cards.inevitableEnd

instance RunMessage InevitableEnd where
  runMessage msg t@(InevitableEnd attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      hand <- iid.hand
      scenarioI18n $ blueDecide iid do
        labeled' "placeDoomOnTheBlueAgenda" $ placeDoomOnFactionAgenda attrs BlueFaction 1
        when (length hand >= 3) do
          labeled' "removeCardsInHandFromGame" do
            chooseNM iid 3 $ targets hand removeCardFromGame
        labeled' "removeTopOfDeckFromGame" $ doStep 1 msg
      pure t
    DoStep 1 (Revelation iid (isSource attrs -> True)) -> do
      cards <- iid.topOfDeckN 6
      let (weaknesses, rest) = partition (`cardMatch` WeaknessCard) (map toCard cards)
      for_ rest removeCardFromGame
      shuffleCardsIntoDeck iid weaknesses
      pure t
    _ -> InevitableEnd <$> liftRunMessage msg attrs
