module Arkham.Event.Events.CallingInFavors (callingInFavors, callingInFavorsEffect, CallingInFavors (..)) where

import Arkham.Asset.Types (Field (..))
import Arkham.Classes.HasQueue (evalQueueT)
import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher hiding (PlayCard)
import Arkham.Projection
import Arkham.Strategy

newtype CallingInFavors = CallingInFavors EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

callingInFavors :: EventCard CallingInFavors
callingInFavors = event CallingInFavors Cards.callingInFavors

instance RunMessage CallingInFavors where
  runMessage msg e@(CallingInFavors attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      choices <- selectForToSnd (#ally <> assetControlledBy iid) \ally -> evalQueueT do
        returnToHand iid ally
        cost <- fieldMap AssetCardDef (.printedCost) ally
        createCardEffect Cards.callingInFavors (effectInt cost) attrs iid
        search iid attrs iid [fromTopOfDeck 9] #ally (PlayFound iid 1)

      chooseOne iid $ map (uncurry targetLabel) choices
      pure e
    _ -> CallingInFavors <$> liftRunMessage msg attrs

newtype CallingInFavorsEffect = CallingInFavorsEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

callingInFavorsEffect :: EffectArgs -> CallingInFavorsEffect
callingInFavorsEffect = cardEffect CallingInFavorsEffect Cards.callingInFavors

instance HasModifiersFor CallingInFavorsEffect where
  getModifiersFor (CallingInFavorsEffect attrs) = do
    case attrs.meta of
      Just (EffectInt n) -> modified_ attrs attrs.target [ReduceCostOf (#asset <> #ally) n]
      _ -> error "Invalid metadata"

instance RunMessage CallingInFavorsEffect where
  runMessage msg e@(CallingInFavorsEffect attrs) = runQueueT $ case msg of
    Discard _ _ (EventTarget eid) | EventSource eid == attrs.source -> disableReturn e
    _ -> CallingInFavorsEffect <$> liftRunMessage msg attrs
