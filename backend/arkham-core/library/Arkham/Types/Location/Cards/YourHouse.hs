{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.YourHouse
  ( YourHouse(..)
  , yourHouse
  )
where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype YourHouse = YourHouse Attrs
  deriving newtype (Show, ToJSON, FromJSON)

yourHouse :: YourHouse
yourHouse = YourHouse $ baseAttrs
  "01124"
  "Your House"
  EncounterSet.TheMidnightMasks
  2
  (PerPlayer 1)
  Squiggle
  [Circle]
  [Arkham]

instance HasModifiersFor env YourHouse where
  getModifiersFor = noModifiersFor

ability :: Attrs -> Ability
ability attrs = (mkAbility (toSource attrs) 1 (ActionAbility 1 Nothing))
  { abilityLimit = PerTurn
  }

instance ActionRunner env => HasActions env YourHouse where
  getActions iid NonFast (YourHouse attrs@Attrs {..}) | locationRevealed = do
    baseActions <- getActions iid NonFast attrs
    unused <- getIsUnused iid (ability attrs)
    canAffordActions <- getCanAffordCost
      iid
      (toSource attrs)
      (ActionCost 1 Nothing locationTraits)
    pure
      $ baseActions
      <> [ ActivateCardAbilityAction iid (ability attrs)
         | unused && iid `member` locationInvestigators && canAffordActions
         ]
  getActions iid window (YourHouse attrs) = getActions iid window attrs

instance (LocationRunner env) => RunMessage env YourHouse where
  runMessage msg l@(YourHouse attrs@Attrs {..}) = case msg of
    Will spawnMsg@(EnemySpawn miid _ eid) -> do
      cardCode <- getId @CardCode eid
      when (cardCode == "01116") $ do
        withQueue $ \queue ->
          ( filter (and . sequence [(/= After spawnMsg), (/= spawnMsg)]) queue
          , ()
          )
        unshiftMessages
          [ EnemySpawn miid locationId eid
          , After (EnemySpawn miid locationId eid)
          ]
      YourHouse <$> runMessage msg attrs
    UseCardAbility iid source _ 1 | isSource attrs source ->
      l <$ unshiftMessages [DrawCards iid 1 False, TakeResources iid 1 False]
    _ -> YourHouse <$> runMessage msg attrs
