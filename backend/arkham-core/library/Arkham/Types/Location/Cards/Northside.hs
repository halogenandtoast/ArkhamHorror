{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.Northside where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Window
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashSet as HashSet

newtype Northside = Northside Attrs
  deriving newtype (Show, ToJSON, FromJSON)

northside :: Northside
northside =
  Northside
    $ (baseAttrs "01134" "Northside" 3 (PerPlayer 2) T [Diamond, Triangle])
        { locationTraits = HashSet.fromList [Arkham]
        , locationVictory = Just 1
        }

instance (ActionRunner env investigator) => HasActions env investigator Northside where
  getActions i NonFast (Northside attrs@Attrs {..}) = do
      baseActions <- getActions i NonFast attrs
      usedAbilities <- map unUsedAbility <$> asks (getList ())
      let
        ability =
          (mkAbility (LocationSource "01134") 1 (ActionAbility 1 Nothing))
            { abilityLimit = PerGame
            }
      pure
        $ baseActions
        <> [ ActivateCardAbilityAction (getId () i) ability
           | resourceCount i >= 5
             && ability `notElem` map snd usedAbilities
             && locationRevealed
             && getId () i `elem` locationInvestigators
             && hasActionsRemaining i
           ] -- GROUP LIMIT
  getActions _ _ _ = pure []

instance (LocationRunner env) => RunMessage env Northside where
  runMessage msg l@(Northside attrs@Attrs {..}) = case msg of
    UseCardAbility iid _ (LocationSource lid) 1 | lid == locationId ->
      l <$ unshiftMessages [SpendResources iid 5, GainClues iid 2]
    _ -> Northside <$> runMessage msg attrs
