module Arkham.Location.Cards.AlkalineForest (alkalineForest) where

import Arkham.Ability
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype AlkalineForest = AlkalineForest LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alkalineForest :: LocationCard AlkalineForest
alkalineForest = locationWith AlkalineForest Cards.alkalineForest 2 (PerPlayer 2) connectsToAdjacent

instance HasAbilities AlkalineForest where
  getAbilities (AlkalineForest a) =
    extendRevealed1 a $ restricted a 1 Here $ forced $ RevealLocation #after You (be a)

instance RunMessage AlkalineForest where
  runMessage msg l@(AlkalineForest attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discardOk <- selectAny $ basic DiscardableCard <> inHandOf NotForPlay iid
      chooseOrRunOneM iid $ withI18n do
        countVar 1 $ labeled' "takeDirectHorror" $ directHorror iid (attrs.ability 1) 1
        countVar 2
          $ labeledValidate' discardOk "discardCardsFromHand"
          $ chooseAndDiscardCards iid (attrs.ability 1) 2
      pure l
    _ -> AlkalineForest <$> liftRunMessage msg attrs
