{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.StudentUnion where

import Arkham.Import

import Arkham.Types.Action
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype StudentUnion = StudentUnion Attrs
  deriving newtype (Show, ToJSON, FromJSON)

studentUnion :: StudentUnion
studentUnion = StudentUnion $ baseAttrs
  "02051"
  "Student Union"
  EncounterSet.ExtracurricularActivity
  1
  (Static 2)
  Diamond
  [Plus, Equals]
  [Miskatonic]

instance HasModifiersFor env StudentUnion where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env StudentUnion where
  getActions iid NonFast (StudentUnion attrs@Attrs {..}) | locationRevealed = do
    baseActions <- getActions iid NonFast attrs
    actionRemainingCount <- unActionRemainingCount
      <$> getCount (Nothing :: Maybe Action, setToList locationTraits, iid)
    let ability = mkAbility (toSource attrs) 1 (ActionAbility 2 Nothing)

    pure
      $ baseActions
      <> [ ActivateCardAbilityAction iid ability
         | iid `elem` locationInvestigators && actionRemainingCount >= 2
         ]
  getActions iid window (StudentUnion attrs) = getActions iid window attrs

instance (LocationRunner env) => RunMessage env StudentUnion where
  runMessage msg l@(StudentUnion attrs) = case msg of
    RevealLocation _ lid | lid == locationId attrs -> do
      unshiftMessage $ PlaceLocationNamed "Dormitories"
      StudentUnion <$> runMessage msg attrs
    UseCardAbility iid source _ 1 | isSource attrs source ->
      l <$ unshiftMessages
        [ HealDamage (InvestigatorTarget iid) 1
        , HealHorror (InvestigatorTarget iid) 1
        ]
    _ -> StudentUnion <$> runMessage msg attrs
