module Arkham.Event.Cards.ObscureStudies
  ( obscureStudies
  , ObscureStudies(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner

newtype ObscureStudies = ObscureStudies EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obscureStudies :: EventCard ObscureStudies
obscureStudies = event ObscureStudies Cards.obscureStudies

instance RunMessage ObscureStudies where
  runMessage msg e@(ObscureStudies attrs) = case msg of
    InvestigatorPlayEvent _ eid _ _ _ | eid == toId attrs -> do
      pure e
    -- PlayThisEvent _iid eid | eid == toId attrs -> do
    --  pure e
    _ -> ObscureStudies <$> runMessage msg attrs
