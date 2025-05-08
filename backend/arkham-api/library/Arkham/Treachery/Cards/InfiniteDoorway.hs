module Arkham.Treachery.Cards.InfiniteDoorway (infiniteDoorway) where

import Arkham.Card
import Arkham.Cost
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype InfiniteDoorway = InfiniteDoorway TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

infiniteDoorway :: TreacheryCard InfiniteDoorway
infiniteDoorway = treachery InfiniteDoorway Cards.infiniteDoorway

instance HasModifiersFor InfiniteDoorway where
  getModifiersFor (InfiniteDoorway attrs) = case attrs.placement of
    AttachedToLocation lid ->
      modified_
        attrs
        lid
        [ AdditionalCostToLeave $ DiscardTopOfDeckWithTargetCost (toTarget attrs) 1
        , AdditionalCostToEnter $ DiscardTopOfDeckWithTargetCost (toTarget attrs) 1
        ]
    _ -> pure mempty

instance RunMessage InfiniteDoorway where
  runMessage msg t@(InfiniteDoorway attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid $ attachTreachery attrs
      pure t
    DiscardedTopOfDeck iid cards _ (isTarget attrs -> True) -> withI18n do
      for_ cards \card -> do
        if card `cardMatch` WeaknessCard
          then drawCard iid card
          else do
            handCards <- select $ inHandOf NotForPlay iid <> basic (CardWithTitle card.title)
            assets <- select $ AssetWithTitle card.title <> assetInPlayAreaOf iid <> DiscardableAsset
            chooseOneAtATimeM iid do
              targets handCards \card' -> chooseOneM iid do
                labeled' "discardFromHand" $ discardCard iid attrs card'
                countVar 1 $ labeled' "takeHorror" $ assignHorror iid attrs 1
              targets assets \asset -> chooseOneM iid do
                labeled' "discardFromPlay" $ toDiscardBy iid attrs asset
                countVar 1 $ labeled' "takeHorror" $ assignHorror iid attrs 1
      pure t
    _ -> InfiniteDoorway <$> liftRunMessage msg attrs
