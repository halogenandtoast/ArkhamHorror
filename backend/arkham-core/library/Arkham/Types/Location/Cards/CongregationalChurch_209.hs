module Arkham.Types.Location.Cards.CongregationalChurch_209
  ( congregationalChurch_209
  , CongregationalChurch_209(..)
  )
where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.Window
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype CongregationalChurch_209 = CongregationalChurch_209 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

congregationalChurch_209 :: CongregationalChurch_209
congregationalChurch_209 = CongregationalChurch_209 $ baseAttrs
  "02209"
  (Name "Congregational Church" Nothing)
  EncounterSet.BloodOnTheAltar
  2
  (PerPlayer 1)
  Diamond
  [Plus, Triangle, Squiggle]
  [Dunwich]

instance HasModifiersFor env CongregationalChurch_209 where
  getModifiersFor = noModifiersFor

ability :: LocationAttrs -> Ability
ability attrs = mkAbility
  (toSource attrs)
  1
  (ActionAbility Nothing
  $ Costs [ActionCost 1, HandDiscardCost 1 Nothing mempty mempty]
  )

instance ActionRunner env => HasActions env CongregationalChurch_209 where
  getActions iid NonFast (CongregationalChurch_209 attrs)
    | locationRevealed attrs = withBaseActions iid NonFast attrs
    $ pure [ActivateCardAbilityAction iid (ability attrs)]
  getActions iid FastPlayerWindow (CongregationalChurch_209 attrs)
    | locationRevealed attrs = withBaseActions iid FastPlayerWindow attrs $ pure
      [ drawCardUnderneathAction iid attrs
      | iid `on` attrs && locationClues attrs == 0
      ]
  getActions iid window (CongregationalChurch_209 attrs) =
    getActions iid window attrs

instance LocationRunner env => RunMessage env CongregationalChurch_209 where
  runMessage msg l@(CongregationalChurch_209 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ unshiftMessage (TakeResources iid 2 False)
    _ -> CongregationalChurch_209 <$> runMessage msg attrs
