module Arkham.Event.Events.EldritchInspiration (
  eldritchInspiration,
  EldritchInspiration (..),
) where

import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Event.Types qualified as Field
import Arkham.Helpers.Effect (lookupEffectCard)
import Arkham.Name
import Arkham.Projection
import Arkham.Timing
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype EldritchInspiration = EldritchInspiration EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eldritchInspiration :: EventCard EldritchInspiration
eldritchInspiration = event EldritchInspiration Cards.eldritchInspiration

instance RunMessage EldritchInspiration where
  runMessage msg e@(EldritchInspiration attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      mmsg <- fromQueue $ find \case
        Do (If wType _) -> case wType of
          Window.RevealChaosTokenEffect {} -> True
          Window.RevealChaosTokenEventEffect {} -> True
          Window.RevealChaosTokenAssetAbilityEffect {} -> True
          _ -> False
        _ -> False

      for_ mmsg $ \effectMsg -> case effectMsg of
        Do (If (Window.RevealChaosTokenEffect _ _ effectId) _) -> do
          mCardDef <- lookupEffectCard effectId
          for_ mCardDef $ \cardDef ->
            questionLabel (display $ cdName cardDef) iid
              $ ChooseOne
                [ Label "Cancel effect" [ResolveEvent iid eid Nothing []]
                , Label "Resolve an additional time" [effectMsg]
                ]
        Do (If (Window.RevealChaosTokenEventEffect _ _ eventId) _) -> do
          cardName <- cdName . toCardDef <$> field Field.EventCard eventId
          questionLabel (display cardName) iid
            $ ChooseOne
              [ Label "Cancel effect" [ResolveEvent iid eid Nothing []]
              , Label "Resolve an additional time" [effectMsg]
              ]
        Do (If (Window.RevealChaosTokenAssetAbilityEffect _ _ assetId) _) -> do
          cardName <- cdName . toCardDef <$> field AssetCard assetId
          questionLabel (display cardName) iid
            $ ChooseOne
              [ Label "Cancel effect" [ResolveEvent iid eid Nothing []]
              , Label "Resolve an additional time" [effectMsg]
              ]
        _ -> error "unhandled"

      pure e
    ResolveEvent _ eid _ _ | eid == toId attrs -> do
      matchingDon't \case
        Do (If wType _) -> case wType of
          Window.RevealChaosTokenEffect {} -> True
          Window.RevealChaosTokenEventEffect {} -> True
          Window.RevealChaosTokenAssetAbilityEffect {} -> True
          _ -> False
        _ -> False
      matchingDon't \case
        CheckWindows [Window AtIf wType _] -> case wType of
          Window.RevealChaosTokenEffect {} -> True
          Window.RevealChaosTokenEventEffect {} -> True
          Window.RevealChaosTokenAssetAbilityEffect {} -> True
          _ -> False
        Do (CheckWindows [Window AtIf wType _]) -> case wType of
          Window.RevealChaosTokenEffect {} -> True
          Window.RevealChaosTokenEventEffect {} -> True
          Window.RevealChaosTokenAssetAbilityEffect {} -> True
          _ -> False
        _ -> False
      cancelledOrIgnoredCardOrGameEffect attrs
      pure e
    _ -> EldritchInspiration <$> liftRunMessage msg attrs
