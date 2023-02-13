module Arkham.Event.Cards.Guidance
  ( guidance
  , Guidance(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message

newtype Guidance = Guidance EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guidance :: EventCard Guidance
guidance = event Guidance Cards.guidance

instance RunMessage Guidance where
  runMessage msg e@(Guidance attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      investigators <-
        selectList $ NotYou <> InvestigatorAt YourLocation <> YetToTakeTurn
      pushAll
        [ chooseOne
          iid
          [ targetLabel
              investigator
              [GainActions investigator (toSource attrs) 1]
          | investigator <- investigators
          ]
        ]
      pure e
    _ -> Guidance <$> runMessage msg attrs
