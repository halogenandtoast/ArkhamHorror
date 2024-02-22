module Arkham.Event.Cards.CallingInFavors (
  callingInFavors,
  callingInFavorsEffect,
  CallingInFavors (..),
) where

import Arkham.Prelude

import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Matcher hiding (PlayCard)
import Arkham.Projection

newtype CallingInFavors = CallingInFavors EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

callingInFavors :: EventCard CallingInFavors
callingInFavors = event CallingInFavors Cards.callingInFavors

instance RunMessage CallingInFavors where
  runMessage msg e@(CallingInFavors attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      allies <- selectList $ #ally <> assetControlledBy iid
      targetsWithCosts <- for allies \ally -> do
        cardDef <- field AssetCardDef ally
        pure (AssetTarget ally, maybe 0 toPrintedCost $ cdCost cardDef)
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ TargetLabel
            target
            [ ReturnToHand iid target
            , createCardEffect Cards.callingInFavors (Just $ EffectInt cost) attrs iid
            , search iid attrs iid [fromTopOfDeck 9] #ally (PlayFound iid 1)
            ]
          | (target, cost) <- targetsWithCosts
          ]
      pure e
    _ -> CallingInFavors <$> runMessage msg attrs

newtype CallingInFavorsEffect = CallingInFavorsEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

callingInFavorsEffect :: EffectArgs -> CallingInFavorsEffect
callingInFavorsEffect = cardEffect CallingInFavorsEffect Cards.callingInFavors

instance HasModifiersFor CallingInFavorsEffect where
  getModifiersFor (InvestigatorTarget iid) (CallingInFavorsEffect attrs) | iid `is` attrs.source = do
    case effectMetadata attrs of
      Just (EffectInt n) -> pure $ toModifiers attrs [ReduceCostOf (#asset <> #ally) n]
      _ -> error "Invalid metadata"
  getModifiersFor _ _ = pure []

instance RunMessage CallingInFavorsEffect where
  runMessage msg e@(CallingInFavorsEffect attrs) = case msg of
    Discard _ _ (EventTarget eid) | EventSource eid == effectSource attrs -> do
      push $ disable attrs
      pure e
    _ -> CallingInFavorsEffect <$> runMessage msg attrs
