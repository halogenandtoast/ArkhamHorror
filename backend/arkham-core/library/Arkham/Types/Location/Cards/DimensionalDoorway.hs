module Arkham.Types.Location.Cards.DimensionalDoorway
  ( dimensionalDoorway
  , DimensionalDoorway(..)
  )
where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Classes
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs hiding (traitsL)
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Trait
import Arkham.Types.Window

newtype DimensionalDoorway = DimensionalDoorway LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimensionalDoorway :: LocationId -> DimensionalDoorway
dimensionalDoorway = DimensionalDoorway . baseAttrs
  "02328"
  "Dimensional Doorway"
  EncounterSet.LostInTimeAndSpace
  2
  (PerPlayer 1)
  Squiggle
  [Triangle, Moon]
  [Otherworld, Extradimensional]

instance HasModifiersFor env DimensionalDoorway where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env DimensionalDoorway where
  getActions iid (AfterEndTurn You) (DimensionalDoorway attrs)
    | iid `on` attrs = pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility (toSource attrs) 1 ForcedAbility)
      ]
  getActions iid window (DimensionalDoorway attrs) =
    getActions iid window attrs

instance LocationRunner env => RunMessage env DimensionalDoorway where
  runMessage msg l@(DimensionalDoorway attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      encounterDiscard <- map unDiscardedEncounterCard <$> getList ()
      let
        mHexCard = find (views traitsL $ member Hex) encounterDiscard
        revelationMsgs = case mHexCard of
          Nothing -> []
          Just hexCard ->
            [ RemoveFromEncounterDiscard hexCard
            , InvestigatorDrewEncounterCard iid hexCard
            ]
      unshiftMessages revelationMsgs
      DimensionalDoorway <$> runMessage msg attrs
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      resourceCount <- unResourceCount <$> getCount iid
      if resourceCount >= 2
        then l <$ unshiftMessage
          (chooseOne
            iid
            [ Label "Spend 2 resource" [SpendResources iid 2]
            , Label
              "Shuffle Dimensional Doorway back into the encounter deck"
              [ShuffleBackIntoEncounterDeck $ toTarget attrs]
            ]
          )
        else l <$ unshiftMessage (ShuffleBackIntoEncounterDeck $ toTarget attrs)
    _ -> DimensionalDoorway <$> runMessage msg attrs
