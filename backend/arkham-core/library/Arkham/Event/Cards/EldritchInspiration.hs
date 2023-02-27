module Arkham.Event.Cards.EldritchInspiration
  ( eldritchInspiration
  , EldritchInspiration(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Types ( Field (..) )
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Effect
import Arkham.Message
import Arkham.Name
import Arkham.Projection
import Arkham.Timing
import Arkham.Window (Window(..))
import Arkham.Window qualified as Window

newtype EldritchInspiration = EldritchInspiration EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eldritchInspiration :: EventCard EldritchInspiration
eldritchInspiration = event EldritchInspiration Cards.eldritchInspiration

instance RunMessage EldritchInspiration where
  runMessage msg e@(EldritchInspiration attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      mmsg <- fromQueue $ find
        (\case
          Do (If wType _) -> case wType of
            Window.RevealTokenEffect{} -> True
            Window.RevealTokenEventEffect{} -> True
            Window.RevealTokenAssetAbilityEffect{} -> True
            _ -> False
          _ -> False
        )

      for_ mmsg $ \effectMsg -> case effectMsg of
        Do (If (Window.RevealTokenEffect _ _ effectId) _) -> do
          mCardDef <- lookupEffectCard effectId
          for_ mCardDef $ \cardDef ->
            push $ questionLabel (display $ toName cardDef) iid $ ChooseOne
              [ Label "Cancel effect" [ResolveEvent iid eid Nothing []]
              , Label "Resolve an additional time" [effectMsg]
              ]
        Do (If (Window.RevealTokenEventEffect _ _ eventId) _) -> do
          cardName <- toName <$> field EventCard eventId
          push $ questionLabel (display cardName) iid $ ChooseOne
            [ Label "Cancel effect" [ResolveEvent iid eid Nothing []]
            , Label "Resolve an additional time" [effectMsg]
            ]
        Do (If (Window.RevealTokenAssetAbilityEffect _ _ assetId) _) -> do
          cardName <- toName <$> field AssetCard assetId
          push $ questionLabel (display cardName) iid $ ChooseOne
            [ Label "Cancel effect" [ResolveEvent iid eid Nothing []]
            , Label "Resolve an additional time" [effectMsg]
            ]
        _ -> error "unhandled"

      pure e
    ResolveEvent _ eid _ _ | eid == toId attrs -> do
      popMessageMatching_
        (\case
          Do (If wType _) -> case wType of
            Window.RevealTokenEffect{} -> True
            Window.RevealTokenEventEffect{} -> True
            Window.RevealTokenAssetAbilityEffect{} -> True
            _ -> False
          _ -> False
        )
      popMessageMatching_
        (\case
          RunWindow _ [Window AtIf wType] -> case wType of
            Window.RevealTokenEffect{} -> True
            Window.RevealTokenEventEffect{} -> True
            Window.RevealTokenAssetAbilityEffect{} -> True
            _ -> False
          _ -> False
        )
      pure e
    _ -> EldritchInspiration <$> runMessage msg attrs
