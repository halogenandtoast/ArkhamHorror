{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.StMarysHospital where

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

newtype StMarysHospital = StMarysHospital Attrs
  deriving newtype (Show, ToJSON, FromJSON)

stMarysHospital :: StMarysHospital
stMarysHospital =
  StMarysHospital
    $ (baseAttrs
        "01128"
        "St. Mary's Hospital"
        2
        (PerPlayer 1)
        Plus
        [Diamond, Square]
      )
        { locationTraits = HashSet.fromList [Arkham]
        }

instance (ActionRunner env investigator) => HasActions env investigator StMarysHospital where
  getActions i NonFast (StMarysHospital attrs@Attrs {..}) = do
    baseActions <- getActions i NonFast attrs
    usedAbilities <- map unUsedAbility <$> asks (getList ())
    let
      ability = (mkAbility (LocationSource "01128") 1 (ActionAbility 1 Nothing)
                )
        { abilityLimit = PerGame
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

instance (LocationRunner env) => RunMessage env StMarysHospital where
  runMessage msg l@(StMarysHospital attrs@Attrs {..}) = case msg of
    UseCardAbility iid (LocationSource lid) _ 1 | lid == locationId ->
      l <$ unshiftMessage (HealDamage (InvestigatorTarget iid) 3)
    _ -> StMarysHospital <$> runMessage msg attrs
