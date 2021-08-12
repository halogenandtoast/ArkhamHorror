module Arkham.Types.Event.Cards.StandTogether3
  ( standTogether3
  , StandTogether3(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Target

newtype StandTogether3 = StandTogether3 EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, TargetEntity)

standTogether3 :: EventCard StandTogether3
standTogether3 = event StandTogether3 Cards.standTogether3

instance HasActions StandTogether3
instance HasModifiersFor env StandTogether3

instance
  ( HasId LocationId env InvestigatorId
  , HasSet InvestigatorId env LocationId
  )
  => RunMessage env StandTogether3 where
  runMessage msg e@(StandTogether3 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == eventId attrs -> do
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
