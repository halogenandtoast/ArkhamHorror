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
  "Overgrown Cairns"
  EncounterSet.CurseOfTheRougarou
  4
  (Static 0)
  Equals
  [Hourglass, Equals]
  [Unhallowed]

instance HasModifiersFor env OvergrownCairns where
  getModifiersFor = noModifiersFor

ability :: Attrs -> Ability
ability attrs = (mkAbility (toSource attrs) 1 (ActionAbility 1 Nothing))
  { abilityLimit = PerGame
  }

instance ActionRunner env => HasActions env OvergrownCairns where
  getActions iid NonFast (OvergrownCairns attrs@Attrs {..}) | locationRevealed =
    do
      unused <- getIsUnused iid (ability attrs)
      baseActions <- getActions iid NonFast attrs
      resourceCount <- getResourceCount iid
      hasActionsRemaining <- getHasActionsRemaining
        iid
        Nothing
        (setToList locationTraits)
      pure
        $ baseActions
        <> [ ActivateCardAbilityAction iid (ability attrs)
           | unused
             && iid
             `member` locationInvestigators
             && resourceCount
             >= 2
             && hasActionsRemaining
           ]
  getActions i window (OvergrownCairns attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env OvergrownCairns where
  runMessage msg l@(OvergrownCairns attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      l <$ unshiftMessages
        [SpendResources iid 2, HealHorror (InvestigatorTarget iid) 2]
    _ -> OvergrownCairns <$> runMessage msg attrs
