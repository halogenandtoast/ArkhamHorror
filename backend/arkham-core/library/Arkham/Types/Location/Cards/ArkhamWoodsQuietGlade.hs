{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.ArkhamWoodsQuietGlade where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Window
import ClassyPrelude
import qualified Data.HashSet as HashSet

newtype ArkhamWoodsQuietGlade = ArkhamWoodsQuietGlade Attrs
  deriving newtype (Show, ToJSON, FromJSON)

arkhamWoodsQuietGlade :: ArkhamWoodsQuietGlade
arkhamWoodsQuietGlade =
  ArkhamWoodsQuietGlade
    $ (baseAttrs
        "01155"
        "Arkham Woods: Quiet Glade"
        1
        (Static 0)
        Square
        [Squiggle]
      )
        { locationTraits = HashSet.fromList [Woods]
        , locationRevealedConnectedSymbols = HashSet.fromList
          [Squiggle, Equals, Hourglass]
        , locationRevealedSymbol = Moon
        }

instance (ActionRunner env investigator) => HasActions env investigator ArkhamWoodsQuietGlade where
  getActions i NonFast (ArkhamWoodsQuietGlade attrs@Attrs {..}) = do
    baseActions <- getActions i NonFast attrs
    usedAbilities <- map unUsedAbility <$> asks (getList ())
    let
      ability = (mkAbility (LocationSource "01155") 1 (ActionAbility 1 Nothing)
                )
        { abilityLimit = PerTurn
        }
    pure
      $ baseActions
      <> [ ActivateCardAbilityAction (getId () i) ability
         | (getId () i, ability)
           `notElem` usedAbilities
           && locationRevealed
           && getId () i
           `elem` locationInvestigators
           && hasActionsRemaining i Nothing locationTraits
         ]
  getActions _ _ _ = pure []

instance (LocationRunner env) => RunMessage env ArkhamWoodsQuietGlade where
  runMessage msg l@(ArkhamWoodsQuietGlade attrs@Attrs {..}) = case msg of
    UseCardAbility iid _ (LocationSource lid) _ 1 | lid == locationId ->
      l <$ unshiftMessages
        [ HealDamage (InvestigatorTarget iid) 1
        , HealHorror (InvestigatorTarget iid) 1
        ]
    _ -> ArkhamWoodsQuietGlade <$> runMessage msg attrs
