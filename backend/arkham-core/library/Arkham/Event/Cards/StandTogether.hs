module Arkham.Event.Cards.StandTogether
  ( standTogether
  , StandTogether(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message

newtype StandTogether = StandTogether EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

standTogether :: EventCard StandTogether
standTogether = event StandTogether Cards.standTogether

instance RunMessage StandTogether where
  runMessage msg e@(StandTogether attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      investigators <-
        selectList
        $ NotInvestigator (InvestigatorWithId iid)
        <> colocatedWith iid
      pushAll
        $ [ chooseOrRunOne
              iid
              [ targetLabel
                  iid'
                  [ TakeResources iid' 2 False
                  , TakeResources iid 2 False
                  , Discard (toTarget attrs)
                  ]
              ]
          | iid' <- investigators
          ]
      pure e
    _ -> StandTogether <$> runMessage msg attrs
