module Arkham.Act.Cards.AtTheExhibitTheRelicsLocation
  ( AtTheExhibitTheRelicsLocation(..)
  , atTheExhibitTheRelicsLocation
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype AtTheExhibitTheRelicsLocation = AtTheExhibitTheRelicsLocation ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

atTheExhibitTheRelicsLocation :: ActCard AtTheExhibitTheRelicsLocation
atTheExhibitTheRelicsLocation = act
  (2, A)
  AtTheExhibitTheRelicsLocation
  Cards.atTheExhibitTheRelicsLocation
  Nothing

instance RunMessage AtTheExhibitTheRelicsLocation where
  runMessage msg (AtTheExhibitTheRelicsLocation attrs) =
    AtTheExhibitTheRelicsLocation <$> runMessage msg attrs
