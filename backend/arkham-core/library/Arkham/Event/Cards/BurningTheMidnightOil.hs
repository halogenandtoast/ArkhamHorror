module Arkham.Event.Cards.BurningTheMidnightOil
  ( burningTheMidnightOil
  , BurningTheMidnightOil(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Location.Types ( Field (..) )
import Arkham.Message
import Arkham.Projection

newtype BurningTheMidnightOil = BurningTheMidnightOil EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burningTheMidnightOil :: EventCard BurningTheMidnightOil
burningTheMidnightOil = event BurningTheMidnightOil Cards.burningTheMidnightOil

instance RunMessage BurningTheMidnightOil where
  runMessage msg e@(BurningTheMidnightOil attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      lid <- getJustLocation iid
      skillType <- field LocationInvestigateSkill lid
      pushAll
        [ TakeResources iid 2 (toSource attrs) False
        , Investigate iid lid (toSource attrs) Nothing skillType False
        ]
      pure e
    _ -> BurningTheMidnightOil <$> runMessage msg attrs
