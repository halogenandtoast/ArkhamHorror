module Arkham.Types.Act.Cards.TheCarnevaleConspiracy
  ( TheCarnevaleConspiracy(..)
  , theCarnevaleConspiracy
  ) where

import Arkham.Prelude

import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.Message

newtype TheCarnevaleConspiracy = TheCarnevaleConspiracy ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

theCarnevaleConspiracy :: TheCarnevaleConspiracy
theCarnevaleConspiracy = TheCarnevaleConspiracy
  $ baseAttrs "82005" "The Carnevale Conspiracy" (Act 1 A) Nothing

instance ActionRunner env => HasActions env TheCarnevaleConspiracy where
  getActions iid window (TheCarnevaleConspiracy x) = getActions iid window x

instance ActRunner env => RunMessage env TheCarnevaleConspiracy where
  runMessage msg a@(TheCarnevaleConspiracy attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      a <$ pushAll [NextAct actId "82006"]
    _ -> TheCarnevaleConspiracy <$> runMessage msg attrs
