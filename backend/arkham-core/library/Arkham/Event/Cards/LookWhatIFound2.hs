module Arkham.Event.Cards.LookWhatIFound2 where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Attrs
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Target

newtype LookWhatIFound2 = LookWhatIFound2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lookWhatIFound2 :: EventCard LookWhatIFound2
lookWhatIFound2 = event LookWhatIFound2 Cards.lookWhatIFound2

instance EventRunner env => RunMessage env LookWhatIFound2 where
  runMessage msg e@(LookWhatIFound2 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      e <$ pushAll
        [ ResolveEvent iid eid Nothing
        , ResolveEvent iid eid Nothing
        , Discard $ toTarget attrs
        ]
    ResolveEvent iid eid _ | eid == toId attrs -> do
      locations <-
        selectList
        $ LocationMatchAny [YourLocation, ConnectedLocation]
        <> LocationWithAnyClues
      e <$ push
        (chooseOne
          iid
          [ TargetLabel
              (LocationTarget lid)
              [InvestigatorDiscoverClues iid lid 1 Nothing]
          | lid <- locations
          ]
        )
    _ -> LookWhatIFound2 <$> runMessage msg attrs
