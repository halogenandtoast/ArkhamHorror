module Arkham.Event.Cards.EldritchInspiration
  ( eldritchInspiration
  , EldritchInspiration(..)
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Message
import Arkham.Name
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
            _ -> False
          _ -> False
        )

      for_ mmsg
        $ \effectMsg@(Do (If (Window.RevealTokenEffect _ _ cardCode) _)) -> do
            let card = lookupCardDef cardCode
            push $ questionLabel (display $ cdName card) iid $ ChooseOne
              [ Label "Cancel effect" [ResolveEvent iid eid Nothing []]
              , Label "Resolve an additional time" [effectMsg]
              ]

      pure e
    ResolveEvent _ eid _ _ | eid == toId attrs -> do
      popMessageMatching_
        (\case
          Do (If wType _) -> case wType of
            Window.RevealTokenEffect{} -> True
            _ -> False
          _ -> False
        )
      pure e
    _ -> EldritchInspiration <$> runMessage msg attrs
