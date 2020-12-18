{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Location.Cards.OvergrownCairns
  ( OvergrownCairns(..)
  , overgrownCairns
  )
where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype OvergrownCairns = OvergrownCairns Attrs
  deriving newtype (Show, ToJSON, FromJSON)

overgrownCairns :: OvergrownCairns
overgrownCairns = OvergrownCairns $ baseAttrs
  "81018"
  (LocationName "Overgrown Cairns" Nothing)
  EncounterSet.CurseOfTheRougarou
  4
  (Static 0)
  Equals
  [Hourglass, Equals]
  [Unhallowed]

instance HasModifiersFor env OvergrownCairns where
  getModifiersFor = noModifiersFor

ability :: Attrs -> Ability
ability attrs = (mkAbility
                  (toSource attrs)
                  1
                  (ActionAbility Nothing $ Costs [ActionCost 1, ResourceCost 2]
                  )
                )
  { abilityLimit = PerGame
  }

instance ActionRunner env => HasActions env OvergrownCairns where
  getActions iid NonFast (OvergrownCairns attrs@Attrs {..}) | locationRevealed =
    withBaseActions iid NonFast attrs $ do
      unused <- getIsUnused iid (ability attrs)
      pure
        [ ActivateCardAbilityAction iid (ability attrs)
        | unused && iid `member` locationInvestigators
        ]
  getActions i window (OvergrownCairns attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env OvergrownCairns where
  runMessage msg l@(OvergrownCairns attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      l <$ unshiftMessages
        [SpendResources iid 2, HealHorror (InvestigatorTarget iid) 2]
    _ -> OvergrownCairns <$> runMessage msg attrs
