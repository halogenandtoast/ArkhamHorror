module Arkham.Types.Location.Cards.BlastedHeath_249
  ( blastedHeath_249
  , BlastedHeath_249(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.Trait

newtype BlastedHeath_249 = BlastedHeath_249 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blastedHeath_249 :: BlastedHeath_249
blastedHeath_249 = BlastedHeath_249 $ baseAttrs
  "02249"
  (Name "Blasted Heath" Nothing)
  EncounterSet.UndimensionedAndUnseen
  3
  (Static 2)
  Square
  [Circle, Hourglass]
  [Dunwich]

instance HasModifiersFor env BlastedHeath_249 where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env BlastedHeath_249 where
  getActions iid window (BlastedHeath_249 attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env BlastedHeath_249 where
  runMessage msg l@(BlastedHeath_249 attrs) = case msg of
    EndTurn iid | iid `on` attrs -> l <$ unshiftMessage
      (InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 0)
    _ -> BlastedHeath_249 <$> runMessage msg attrs
