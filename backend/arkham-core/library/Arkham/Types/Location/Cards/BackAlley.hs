{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.BackAlley
  ( backAlley
  , BackAlley(..)
  )
where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Game.Helpers
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait hiding (Cultist)

newtype BackAlley = BackAlley Attrs
  deriving newtype (Show, ToJSON, FromJSON)

backAlley :: BackAlley
backAlley = BackAlley
  $ base { locationVictory = Just 1, locationRevealedSymbol = Squiggle }
 where
  base = baseAttrs
    "02077"
    (LocationName "Back Alley" Nothing)
    EncounterSet.TheHouseAlwaysWins
    1
    (PerPlayer 1)
    T
    [Diamond]
    [CloverClub]

instance HasModifiersFor env BackAlley where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env BackAlley where
  getActions iid NonFast (BackAlley attrs@Attrs {..}) | locationRevealed = do
    baseActions <- getActions iid NonFast attrs
    canAffordActions <- getCanAffordCost
      iid
      (toSource attrs)
      (ActionCost 1 (Just Action.Resign) locationTraits)
    pure
      $ baseActions
      <> [ ActivateCardAbilityAction
             iid
             (mkAbility
               (toSource attrs)
               1
               (ActionAbility 1 (Just Action.Resign))
             )
         | iid `member` locationInvestigators && canAffordActions
         ]
  getActions iid window (BackAlley attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env BackAlley where
  runMessage msg l@(BackAlley attrs@Attrs {..}) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source && locationRevealed ->
      l <$ unshiftMessage (Resign iid)
    _ -> BackAlley <$> runMessage msg attrs
