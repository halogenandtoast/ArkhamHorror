{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.TrappersCabin where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype TrappersCabin = TrappersCabin Attrs
  deriving newtype (Show, ToJSON, FromJSON)

trappersCabin :: TrappersCabin
trappersCabin = TrappersCabin $ baseAttrs
  "81014"
  "Trapper's Cabin"
  EncounterSet.CurseOfTheRougarou
  3
  (Static 0)
  Moon
  [Diamond, Moon]
  [Wilderness]

instance HasModifiersFor env TrappersCabin where
  getModifiersFor _ (InvestigatorTarget iid) (TrappersCabin attrs) =
    pure [ CannotGainResources | iid `member` locationInvestigators attrs ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env TrappersCabin where
  getActions iid NonFast (TrappersCabin attrs@Attrs {..}) | locationRevealed =
    do
      baseActions <- getActions iid NonFast attrs
      resourceCount <- getResourceCount iid
      hasActionsRemaining <- getHasActionsRemaining
        iid
        Nothing
        (setToList locationTraits)
      pure
        $ baseActions
        <> [ ActivateCardAbilityAction
               iid
               (mkAbility (toSource attrs) 1 (ActionAbility 1 Nothing))
           | iid
             `member` locationInvestigators
             && resourceCount
             >= 5
             && hasActionsRemaining
           ]
  getActions i window (TrappersCabin attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env TrappersCabin where
  runMessage msg l@(TrappersCabin attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> l <$ unshiftMessage
      (BeginSkillTest iid source (toTarget attrs) Nothing SkillIntellect 3)
    PassedSkillTest iid _ source _ _ | isSource attrs source ->
      l <$ unshiftMessage (TakeControlOfSetAsideAsset iid "81029")
    _ -> TrappersCabin <$> runMessage msg attrs
