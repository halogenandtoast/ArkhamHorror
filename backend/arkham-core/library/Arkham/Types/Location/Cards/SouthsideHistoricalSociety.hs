{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.SouthsideHistoricalSociety where

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
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashSet as HashSet

newtype SouthsideHistoricalSociety = SouthsideHistoricalSociety Attrs
  deriving newtype (Show, ToJSON, FromJSON)

southsideHistoricalSociety :: SouthsideHistoricalSociety
southsideHistoricalSociety =
  SouthsideHistoricalSociety
    $ (baseAttrs
        "01126"
        "Southside"
        3
        (PerPlayer 1)
        Square
        [Diamond, Plus, Circle]
      )
        { locationTraits = HashSet.fromList [Arkham]
        }

instance (ActionRunner env investigator) => HasActions env investigator SouthsideHistoricalSociety where
  getActions i NonFast (SouthsideHistoricalSociety attrs@Attrs {..}) = do
      baseActions <- getActions i NonFast attrs
      usedAbilities <- map unUsedAbility <$> asks (getList ())
      let
        ability =
          (mkAbility (LocationSource "01126") 1 (ActionAbility 1 Nothing))
            { abilityLimit = OncePerGame
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

instance (LocationRunner env) => RunMessage env SouthsideHistoricalSociety where
  runMessage msg l@(SouthsideHistoricalSociety attrs@Attrs {..}) = case msg of
    UseCardAbility iid _ (LocationSource lid) 1 | lid == locationId ->
      l <$ unshiftMessage (DrawCards iid 3 False)
    _ -> SouthsideHistoricalSociety <$> runMessage msg attrs
