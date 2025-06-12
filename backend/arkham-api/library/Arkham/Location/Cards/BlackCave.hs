module Arkham.Location.Cards.BlackCave (blackCave) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.I18n
import Arkham.Investigator.Types
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype BlackCave = BlackCave LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blackCave :: LocationCard BlackCave
blackCave = location BlackCave Cards.blackCave 3 (PerPlayer 2)

instance HasAbilities BlackCave where
  getAbilities (BlackCave a) = extendRevealed1 a $ restricted a 1 Here $ forced $ TurnEnds #when You

instance RunMessage BlackCave where
  runMessage msg l@(BlackCave attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cardsInHand <- fieldMap InvestigatorHand length iid
      chooseOrRunOneM iid $ withI18n do
        countVar 1 $ labeled' "takeHorror" $ assignHorror iid (attrs.ability 1) 1
        when (cardsInHand >= 2) do
          countVar 2 $ labeled' "discardCardsFromHand" $ chooseAndDiscardCards iid (attrs.ability 1) 2
      pure l
    _ -> BlackCave <$> liftRunMessage msg attrs
