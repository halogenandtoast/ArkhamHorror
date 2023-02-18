module Arkham.Act.Cards.CampusSafety where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Message
import Arkham.Resolution

newtype CampusSafety = CampusSafety ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

campusSafety :: ActCard CampusSafety
campusSafety = act (3, A) CampusSafety Cards.campusSafety Nothing

-- | Missing HasAbilities?
-- Campus Safety has an Objective but it is triggered by other cards so this is
-- left off of this definition

instance RunMessage CampusSafety where
  runMessage msg a@(CampusSafety attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      push $ scenarioResolution 3
      pure a
    _ -> CampusSafety <$> runMessage msg attrs
