{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.YourHouse where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Card.CardCode
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

newtype YourHouse = YourHouse Attrs
  deriving newtype (Show, ToJSON, FromJSON)

yourHouse :: YourHouse
yourHouse =
  YourHouse $ (baseAttrs "01124" "Your House" 2 (PerPlayer 1) Squiggle [Circle])
    { locationTraits = HashSet.fromList [Arkham]
    }

instance (ActionRunner env investigator) => HasActions env investigator YourHouse where
  getActions i NonFast (YourHouse attrs@Attrs {..}) = do
      baseActions <- getActions i NonFast attrs
      usedAbilities <- map unUsedAbility <$> asks (getList ())
      let
        ability =
          (mkAbility (LocationSource "01124") 1 (ActionAbility 1 Nothing))
            { abilityLimit = OncePerTurn
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

instance (LocationRunner env) => RunMessage env YourHouse where
  runMessage msg l@(YourHouse attrs@Attrs {..}) = case msg of
    Will (EnemySpawn _ eid) -> do
      cardCode <- asks (getId @CardCode eid)
      when (cardCode == "01116") $ do
        void popMessage
        unshiftMessage (EnemySpawn "01124" eid)
      YourHouse <$> runMessage msg attrs
    UseCardAbility iid _ (LocationSource lid) 1 | lid == locationId ->
      l <$ unshiftMessages [DrawCards iid 1 False, TakeResources iid 1 False]
    _ -> YourHouse <$> runMessage msg attrs
