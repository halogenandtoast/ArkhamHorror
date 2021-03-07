module Arkham.Types.Act.Cards.OutOfThisWorld
  ( OutOfThisWorld(..)
  , outOfThisWorld
  )
where

import Arkham.Prelude

import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message

newtype OutOfThisWorld = OutOfThisWorld ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

outOfThisWorld :: OutOfThisWorld
outOfThisWorld = OutOfThisWorld $ baseAttrs
  "02316"
  "Out of this World"
  (Act 1 A)
  (Just $ RequiredClues (PerPlayer 2) Nothing)

instance ActionRunner env => HasActions env OutOfThisWorld where
  getActions i window (OutOfThisWorld x) = getActions i window x

instance ActRunner env => RunMessage env OutOfThisWorld where
  runMessage msg a@(OutOfThisWorld attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide B attrs ->
      a <$ unshiftMessage (NextAct actId "02317")
    _ -> OutOfThisWorld <$> runMessage msg attrs
