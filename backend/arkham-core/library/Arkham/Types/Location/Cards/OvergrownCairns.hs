{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.OvergrownCairns
  ( OvergrownCairns(..)
  , overgrownCairns
  )
where

import Arkham.Import

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
  4
  (Static 0)
  Equals
  [Hourglass, Equals]
  [Unhallowed]

instance HasModifiersFor env investigator OvergrownCairns where
  getModifiersFor _ _ _ = pure []

ability :: Attrs -> Ability
ability attrs = mkAbility (toSource attrs) 1 (ActionAbility 1 Nothing)

instance (ActionRunner env investigator) => HasActions env investigator OvergrownCairns where
  getActions i NonFast (OvergrownCairns attrs@Attrs {..}) | locationRevealed =
    do
      unused <- getIsUnused i (ability attrs)
      baseActions <- getActions i NonFast attrs
      pure
        $ baseActions
        <> [ ActivateCardAbilityAction (getId () i) (ability attrs)
           | unused
             && atLocation i attrs
             && resourceCount i
             >= 2
             && hasActionsRemaining i Nothing locationTraits
           ]
  getActions i window (OvergrownCairns attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env OvergrownCairns where
  runMessage msg l@(OvergrownCairns attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      l <$ unshiftMessages
        [SpendResources iid 2, HealHorror (InvestigatorTarget iid) 2]
    _ -> OvergrownCairns <$> runMessage msg attrs
