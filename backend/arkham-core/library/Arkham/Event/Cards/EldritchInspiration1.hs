module Arkham.Event.Cards.EldritchInspiration1 (
  eldritchInspiration1,
  EldritchInspiration1 (..),
) where

import Arkham.Prelude

import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Effect
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
  runMessage msg e@(EldritchInspiration1 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      mmsg <- fromQueue $ find \case
        Do (If wType _) -> case wType of
          Window.RevealChaosTokenEffect {} -> True
          Window.RevealChaosTokenEventEffect {} -> True
          Window.RevealChaosTokenAssetAbilityEffect {} -> True
          _ -> False
        _ -> False

      player <- getPlayer iid

      for_ mmsg $ \effectMsg -> case effectMsg of
        Do (If (Window.RevealChaosTokenEffect _ _ effectId) _) -> do
          mCardDef <- lookupEffectCard effectId
          for_ mCardDef $ \cardDef ->
            push
              $ questionLabel (display $ cdName cardDef) player
              $ ChooseOne
                [ Label "Cancel effect" [ResolveEvent iid eid Nothing []]
                , Label "Resolve an additional time" [effectMsg]
                ]
        Do (If (Window.RevealChaosTokenEventEffect _ _ eventId) _) -> do
          cardName <- cdName . toCardDef <$> field EventCard eventId
          push
            $ questionLabel (display cardName) player
            $ ChooseOne
              [ Label "Cancel effect" [ResolveEvent iid eid Nothing []]
              , Label "Resolve an additional time" [effectMsg]
              ]
        Do (If (Window.RevealChaosTokenAssetAbilityEffect _ _ assetId) _) -> do
          cardName <- cdName . toCardDef <$> field AssetCard assetId
          push
            $ questionLabel (display cardName) player
            $ ChooseOne
              [ Label "Cancel effect" [ResolveEvent iid eid Nothing []]
              , Label "Resolve an additional time" [effectMsg]
              ]
        _ -> error "unhandled"

      pure e
    ResolveEvent _ eid _ _ | eid == toId attrs -> do
      popMessageMatching_ \case
        Do (If wType _) -> case wType of
          Window.RevealChaosTokenEffect {} -> True
          Window.RevealChaosTokenEventEffect {} -> True
          Window.RevealChaosTokenAssetAbilityEffect {} -> True
          _ -> False
        _ -> False

      popMessageMatching_ \case
        RunWindow _ [Window AtIf wType _] -> case wType of
          Window.RevealChaosTokenEffect {} -> True
          Window.RevealChaosTokenEventEffect {} -> True
          Window.RevealChaosTokenAssetAbilityEffect {} -> True
          _ -> False
        _ -> False
      pure e
    _ -> EldritchInspiration1 <$> runMessage msg attrs
