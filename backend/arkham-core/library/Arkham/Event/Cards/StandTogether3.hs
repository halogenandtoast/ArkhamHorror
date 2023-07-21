module Arkham.Event.Cards.StandTogether3 (
  standTogether3,
  StandTogether3 (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message

newtype StandTogether3 = StandTogether3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Targetable)

standTogether3 :: EventCard StandTogether3
standTogether3 = event StandTogether3 Cards.standTogether3

instance RunMessage StandTogether3 where
  runMessage msg e@(StandTogether3 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId attrs -> do
      investigatorIds <- filter (/= iid) <$> selectList (colocatedWith iid)
      drawing1 <- drawCards iid attrs 2
      case investigatorIds of
        [] -> error "should not have happened"
        xs -> do
          investigators <- forToSnd xs $ \x -> drawCards x attrs 2
          pushAll
            [ chooseOrRunOne
                iid
                [ TargetLabel
                  (InvestigatorTarget x)
                  [ TakeResources iid 2 (toSource attrs) False
                  , TakeResources x 2 (toSource attrs) False
                  , drawing1
                  , drawing2
                  ]
                | (x, drawing2) <- investigators
                ]
            ]
      pure e
    _ -> StandTogether3 <$> runMessage msg attrs
