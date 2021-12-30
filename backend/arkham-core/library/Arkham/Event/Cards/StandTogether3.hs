module Arkham.Event.Cards.StandTogether3
  ( standTogether3
  , StandTogether3(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Attrs
import Arkham.Id
import Arkham.Message
import Arkham.Target

newtype StandTogether3 = StandTogether3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, TargetEntity)

standTogether3 :: EventCard StandTogether3
standTogether3 = event StandTogether3 Cards.standTogether3

instance
  ( HasId LocationId env InvestigatorId
  , HasSet InvestigatorId env LocationId
  )
  => RunMessage env StandTogether3 where
  runMessage msg e@(StandTogether3 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId attrs -> do
      locationId <- getId @LocationId iid
      investigatorIds <- filter (/= iid)
        <$> getSetList @InvestigatorId locationId
      case investigatorIds of
        [] -> error "should not have happened"
        [x] -> e <$ pushAll
          [ TakeResources iid 2 False
          , TakeResources x 2 False
          , DrawCards iid 2 False
          , DrawCards x 2 False
          , Discard (toTarget e)
          ]
        xs -> e <$ pushAll
          [ chooseOne
            iid
            [ TargetLabel
                (InvestigatorTarget x)
                [ TakeResources iid 2 False
                , TakeResources x 2 False
                , DrawCards iid 2 False
                , DrawCards x 2 False
                ]
            | x <- xs
            ]
          , Discard (toTarget e)
          ]
    _ -> StandTogether3 <$> runMessage msg attrs
