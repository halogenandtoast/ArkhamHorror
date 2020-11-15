{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.MiskatonicUniversityMiskatonicMuseum
  ( MiskatonicUniversityMiskatonicMuseum(..)
  , miskatonicUniversityMiskatonicMuseum
  )
where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype MiskatonicUniversityMiskatonicMuseum = MiskatonicUniversityMiskatonicMuseum Attrs
  deriving newtype (Show, ToJSON, FromJSON)

miskatonicUniversityMiskatonicMuseum :: MiskatonicUniversityMiskatonicMuseum
miskatonicUniversityMiskatonicMuseum =
  MiskatonicUniversityMiskatonicMuseum
    $ (baseAttrs
        "50029"
        "Miskatonic University"
        EncounterSet.ReturnToTheMidnightMasks
        3
        (PerPlayer 1)
        Diamond
        [T, Plus, Circle, Square]
        [Arkham]
      )
        { locationVictory = Just 1
        }

instance HasModifiersFor env MiskatonicUniversityMiskatonicMuseum where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env MiskatonicUniversityMiskatonicMuseum where
  getActions iid NonFast (MiskatonicUniversityMiskatonicMuseum attrs@Attrs {..})
    | locationRevealed = do
      baseActions <- getActions iid NonFast attrs
      hasActionsRemaining <- getHasActionsRemaining
        iid
        Nothing
        (setToList locationTraits)
      pure
        $ baseActions
        <> [ ActivateCardAbilityAction
               iid
               (mkAbility (toSource attrs) 1 (ActionAbility 1 Nothing))
           | iid `member` locationInvestigators && hasActionsRemaining
           ]
  getActions _ _ _ = pure []

instance (LocationRunner env) => RunMessage env MiskatonicUniversityMiskatonicMuseum where
  runMessage msg l@(MiskatonicUniversityMiskatonicMuseum attrs@Attrs {..}) =
    case msg of
      UseCardAbility iid source _ 1 | isSource attrs source ->
        l <$ unshiftMessages
          [InvestigatorAssignDamage iid source 0 2, GainClues iid 1]
      _ -> MiskatonicUniversityMiskatonicMuseum <$> runMessage msg attrs
