module Arkham.Treachery.Cards.KindredMist (kindredMist, KindredMist (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Modifier
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype KindredMist = KindredMist TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kindredMist :: TreacheryCard KindredMist
kindredMist = treachery KindredMist Cards.kindredMist

instance HasAbilities KindredMist where
  getAbilities (KindredMist a) = [restricted a 1 OnSameLocation $ forced $ ScenarioEvent #when "shuffleTekelili"]

fromScenarioEvent :: (HasCallStack, FromJSON a) => Text -> [Window] -> a
fromScenarioEvent _ [] = error "No such scenario event"
fromScenarioEvent k ((windowType -> Window.ScenarioEvent k' json) : _) | k == k' = toResult json
fromScenarioEvent k (_ : rest) = fromScenarioEvent k rest

instance RunMessage KindredMist where
  runMessage msg t@(KindredMist attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      ls <- select $ NearestLocationTo iid $ LocationWithoutTreachery (treacheryIs Cards.kindredMist)
      chooseTargetM iid ls $ place attrs . AttachedToLocation
      pure t
    UseCardAbility
      iid
      (isSource attrs -> True)
      1
      (fromScenarioEvent "shuffleTekelili" -> (batchId, cards :: [Card]))
      _ -> do
        cancelBatch batchId
        for_ (onlyPlayerCards cards) \card -> do
          -- A bit of an issue here since normally adding shuffle the deck we need to use this variant
          push $ AddCardToDeckForCampaign iid card
          cardResolutionModifier card (attrs.ability 1) card PlaceOnBottomOfDeckInsteadOfDiscard
          drawCard iid (toCard card)
        atEndOfRound (attrs.ability 1) $ toDiscard (attrs.ability 1) attrs
        pure t
    _ -> KindredMist <$> liftRunMessage msg attrs
