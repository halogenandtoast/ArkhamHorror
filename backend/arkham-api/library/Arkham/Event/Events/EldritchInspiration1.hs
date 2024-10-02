module Arkham.Event.Events.EldritchInspiration1 (
  eldritchInspiration1,
  EldritchInspiration1 (..),
) where

import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Event.Types qualified as Field
import Arkham.Helpers.Effect (lookupEffectCard)
import Arkham.Name
import Arkham.Projection
import Arkham.Timing
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype EldritchInspiration1 = EldritchInspiration1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eldritchInspiration1 :: EventCard EldritchInspiration1
eldritchInspiration1 = event EldritchInspiration1 Cards.eldritchInspiration1

instance RunMessage EldritchInspiration1 where
  runMessage msg e@(EldritchInspiration1 attrs) = runQueueT $ case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
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
    _ -> EldritchInspiration1 <$> liftRunMessage msg attrs
