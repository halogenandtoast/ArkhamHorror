{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.SouthsideHistoricalSociety
  ( SouthsideHistoricalSociety(..)
  , southsideHistoricalSociety
  )
where

import Arkham.Import

import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype SouthsideHistoricalSociety = SouthsideHistoricalSociety Attrs
  deriving newtype (Show, ToJSON, FromJSON)

southsideHistoricalSociety :: SouthsideHistoricalSociety
southsideHistoricalSociety = SouthsideHistoricalSociety $ baseAttrs
  "01126"
  "Southside"
  3
  (PerPlayer 1)
  Square
  [Diamond, Plus, Circle]
  [Arkham]

instance HasModifiersFor env investigator SouthsideHistoricalSociety where
  getModifiersFor _ _ _ = pure []

ability :: Attrs -> Ability
ability attrs = (mkAbility (toSource attrs) 1 (ActionAbility 1 Nothing))
  { abilityLimit = PerGame
  }

instance (ActionRunner env investigator) => HasActions env investigator SouthsideHistoricalSociety where
  getActions i NonFast (SouthsideHistoricalSociety attrs@Attrs {..})
    | locationRevealed = do
      baseActions <- getActions i NonFast attrs
      unused <- getIsUnused i (ability attrs)
      let
      pure
        $ baseActions
        <> [ ActivateCardAbilityAction (getId () i) (ability attrs)
           | unused
             && atLocation i attrs
             && hasActionsRemaining i Nothing locationTraits
           ]
  getActions _ _ _ = pure []

instance (LocationRunner env) => RunMessage env SouthsideHistoricalSociety where
  runMessage msg l@(SouthsideHistoricalSociety attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      l <$ unshiftMessage (DrawCards iid 3 False)
    _ -> SouthsideHistoricalSociety <$> runMessage msg attrs
