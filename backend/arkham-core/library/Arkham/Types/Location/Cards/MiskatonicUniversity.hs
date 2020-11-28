{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.MiskatonicUniversity where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype MiskatonicUniversity = MiskatonicUniversity Attrs
  deriving newtype (Show, ToJSON, FromJSON)

miskatonicUniversity :: MiskatonicUniversity
miskatonicUniversity = MiskatonicUniversity $ (baseAttrs
                                                "01129"
                                                "Miskatonic University"
                                                EncounterSet.TheMidnightMasks
                                                4
                                                (PerPlayer 2)
                                                Diamond
                                                [T, Plus, Circle, Square]
                                                [Arkham]
                                              )
  { locationVictory = Just 1
  }

instance HasModifiersFor env MiskatonicUniversity where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env MiskatonicUniversity where
  getActions iid NonFast (MiskatonicUniversity attrs@Attrs {..})
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
               (mkAbility (LocationSource "01129") 1 (ActionAbility 1 Nothing))
           | iid `member` locationInvestigators && hasActionsRemaining
           ]
  getActions iid window (MiskatonicUniversity attrs) =
    getActions iid window attrs

instance (LocationRunner env) => RunMessage env MiskatonicUniversity where
  runMessage msg l@(MiskatonicUniversity attrs@Attrs {..}) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> l <$ unshiftMessage
      (SearchTopOfDeck
        iid
        (InvestigatorTarget iid)
        6
        [Tome, Spell]
        ShuffleBackIn
      )
    _ -> MiskatonicUniversity <$> runMessage msg attrs
