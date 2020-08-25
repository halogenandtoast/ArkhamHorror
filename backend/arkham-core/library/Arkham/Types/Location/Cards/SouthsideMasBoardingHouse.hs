{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.SouthsideMasBoardingHouse where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.FastWindow
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashSet as HashSet

newtype SouthsideMasBoardingHouse = SouthsideMasBoardingHouse Attrs
  deriving newtype (Show, ToJSON, FromJSON)

southsideMasBoardingHouse :: SouthsideMasBoardingHouse
southsideMasBoardingHouse =
  SouthsideMasBoardingHouse
    $ (baseAttrs
        "01127"
        "Southside"
        2
        (PerPlayer 1)
        Square
        [Diamond, Plus, Circle]
      )
        { locationTraits = HashSet.fromList [Arkham]
        }

instance (ActionRunner env investigator) => HasActions env investigator SouthsideMasBoardingHouse where
  getActions i NonFast (SouthsideMasBoardingHouse attrs@Attrs {..}) = do
      baseActions <- getActions i NonFast attrs
      usedAbilities <- map unUsedAbility <$> asks (getList ())
      let
        ability =
          (mkAbility (LocationSource "01127") 1 (ActionAbility 1 Nothing))
            { abilityLimit = PerGame
            }
      pure
        $ baseActions
        <> [ ActivateCardAbilityAction (getId () i) ability
           | (getId () i, ability) `notElem` usedAbilities
             && locationRevealed
             && getId () i `elem` locationInvestigators
             && hasActionsRemaining i
           ]
  getActions _ _ _ = pure []

instance (LocationRunner env) => RunMessage env SouthsideMasBoardingHouse where
  runMessage msg l@(SouthsideMasBoardingHouse attrs@Attrs {..}) = case msg of
    UseCardAbility iid _ (LocationSource lid) 1 | lid == locationId ->
      l <$ unshiftMessage
        (SearchDeckForTraits iid (InvestigatorTarget iid) [Ally])
    _ -> SouthsideMasBoardingHouse <$> runMessage msg attrs
