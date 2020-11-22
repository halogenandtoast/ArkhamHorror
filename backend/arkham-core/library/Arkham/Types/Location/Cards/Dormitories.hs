{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.Dormitories where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Game.Helpers
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype Dormitories = Dormitories Attrs
  deriving newtype (Show, ToJSON, FromJSON)

dormitories :: Dormitories
dormitories = Dormitories $ (baseAttrs
                              "02048"
                              "Dormitories"
                              EncounterSet.ExtracurricularActivity
                              1
                              (PerPlayer 3)
                              Equals
                              [Diamond]
                              [Miskatonic]
                            )
  { locationVictory = Just 1
  }

instance HasModifiersFor env Dormitories where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Dormitories where
  getActions iid FastPlayerWindow (Dormitories attrs@Attrs {..})
    | locationRevealed = do
      baseActions <- getActions iid FastPlayerWindow attrs
      requiredClueCount <- getPlayerCountValue (PerPlayer 3)
      totalSpendableClueCount <- getSpendableClueCount
        (setToList locationInvestigators)
      pure
        $ baseActions
        <> [ ActivateCardAbilityAction
               iid
               (mkAbility
                 (toSource attrs)
                 1
                 (ActionAbility 1 (Just Action.Resign))
               )
           | totalSpendableClueCount >= requiredClueCount
           ]
  getActions iid window (Dormitories attrs) = getActions iid window attrs

instance (LocationRunner env) => RunMessage env Dormitories where
  runMessage msg l@(Dormitories attrs) = case msg of
    UseCardAbility _iid source _ 1
      | isSource attrs source && locationRevealed attrs -> do
        requiredClueCount <- getPlayerCountValue (PerPlayer 3)
        l <$ unshiftMessages
          [ SpendClues
            requiredClueCount
            (setToList $ locationInvestigators attrs)
          , Resolution 2
          ]
    _ -> Dormitories <$> runMessage msg attrs
