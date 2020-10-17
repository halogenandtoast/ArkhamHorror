{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.ArkhamWoodsQuietGlade
  ( ArkhamWoodsQuietGlade(..)
  , arkhamWoodsQuietGlade
  )
where

import Arkham.Import

import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype ArkhamWoodsQuietGlade = ArkhamWoodsQuietGlade Attrs
  deriving newtype (Show, ToJSON, FromJSON)

arkhamWoodsQuietGlade :: ArkhamWoodsQuietGlade
arkhamWoodsQuietGlade = ArkhamWoodsQuietGlade $ base
  { locationRevealedConnectedSymbols = setFromList [Squiggle, Equals, Hourglass]
  , locationRevealedSymbol = Moon
  }
 where
  base = baseAttrs
    "01155"
    "Arkham Woods: Quiet Glade"
    1
    (Static 0)
    Square
    [Squiggle]
    [Woods]

instance HasModifiersFor env ArkhamWoodsQuietGlade where
  getModifiersFor _ _ _ = pure []

ability :: Attrs -> Ability
ability attrs = (mkAbility (toSource attrs) 1 (ActionAbility 1 Nothing))
  { abilityLimit = PerTurn
  }

instance ActionRunner env => HasActions env ArkhamWoodsQuietGlade where
  getActions iid NonFast (ArkhamWoodsQuietGlade attrs@Attrs {..})
    | locationRevealed = do
      baseActions <- getActions iid NonFast attrs
      unused <- getIsUnused iid (ability attrs)
      hasActionsRemaining <- getHasActionsRemaining
        iid
        Nothing
        (setToList locationTraits)

      pure
        $ baseActions
        <> [ ActivateCardAbilityAction iid (ability attrs)
           | unused && iid `elem` locationInvestigators && hasActionsRemaining
           ]
  getActions _ _ _ = pure []

instance (LocationRunner env) => RunMessage env ArkhamWoodsQuietGlade where
  runMessage msg l@(ArkhamWoodsQuietGlade attrs@Attrs {..}) = case msg of
    UseCardAbility iid (LocationSource lid) _ 1 | lid == locationId ->
      l <$ unshiftMessages
        [ HealDamage (InvestigatorTarget iid) 1
        , HealHorror (InvestigatorTarget iid) 1
        ]
    _ -> ArkhamWoodsQuietGlade <$> runMessage msg attrs
