{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.AlchemyLabs
  ( alchemyLabs
  , AlchemyLabs(..)
  )
where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Game.Helpers
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype AlchemyLabs = AlchemyLabs Attrs
  deriving newtype (Show, ToJSON, FromJSON)

alchemyLabs :: AlchemyLabs
alchemyLabs = AlchemyLabs $ baseAttrs
  "02057"
  "Alchemy Labs"
  EncounterSet.ExtracurricularActivity
  5
  (Static 0)
  Squiggle
  [Hourglass]
  [Miskatonic]

instance HasModifiersFor env AlchemyLabs where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env AlchemyLabs where
  getActions iid NonFast (AlchemyLabs attrs@Attrs {..}) | locationRevealed = do
    baseActions <- getActions iid NonFast attrs
    hasActionsRemaining <- getHasActionsRemaining
      iid
      Nothing
      (setToList locationTraits)

    let
      ability =
        mkAbility (toSource attrs) 1 (ActionAbility 1 (Just Action.Investigate))

    pure
      $ baseActions
      <> [ ActivateCardAbilityAction iid ability
         | iid `elem` locationInvestigators && hasActionsRemaining
         ]
  getActions _ _ _ = pure []

instance LocationRunner env => RunMessage env AlchemyLabs where
  runMessage msg (AlchemyLabs attrs) = AlchemyLabs <$> runMessage msg attrs
