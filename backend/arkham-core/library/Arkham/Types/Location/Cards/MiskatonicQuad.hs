{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Location.Cards.MiskatonicQuad
  ( MiskatonicQuad(..)
  , miskatonicQuad
  )
where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Game.Helpers
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype MiskatonicQuad = MiskatonicQuad Attrs
  deriving newtype (Show, ToJSON, FromJSON)

miskatonicQuad :: MiskatonicQuad
miskatonicQuad = MiskatonicQuad $ baseAttrs
  "02048"
  (LocationName "Miskatonic Quad" Nothing)
  EncounterSet.ExtracurricularActivity
  3
  (Static 0)
  Plus
  [Triangle, Hourglass, Square, Diamond, Circle]
  [Miskatonic]

instance HasModifiersFor env MiskatonicQuad where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env MiskatonicQuad where
  getActions iid NonFast (MiskatonicQuad attrs@Attrs {..}) | locationRevealed =
    withBaseActions iid NonFast attrs $ do
      canAffordActions <- getCanAffordCost
        iid
        (toSource attrs)
        (Just Action.Resign)
        (ActionCost 1)
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility
              (toSource attrs)
              1
              (ActionAbility (Just Action.Resign) (ActionCost 1))
            )
        | iid `member` locationInvestigators && canAffordActions
        ]
  getActions iid window (MiskatonicQuad attrs) = getActions iid window attrs

instance (LocationRunner env) => RunMessage env MiskatonicQuad where
  runMessage msg l@(MiskatonicQuad attrs) = case msg of
    UseCardAbility iid source _ 1
      | isSource attrs source && locationRevealed attrs -> l
      <$ unshiftMessage (Resign iid)
    _ -> MiskatonicQuad <$> runMessage msg attrs
