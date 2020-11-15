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
    hasActionsRemaining <- getHasActionsRemaining
      iid
      Nothing
      (setToList locationTraits)
    pure
      $ baseActions
      <> [ ActivateCardAbilityAction iid (ability attrs)
         | unused && iid `member` locationInvestigators && hasActionsRemaining
         ]
  getActions _ _ _ = pure []

instance (LocationRunner env) => RunMessage env YourHouse where
  runMessage msg l@(YourHouse attrs@Attrs {..}) = case msg of
    Will (EnemySpawn miid _ eid) -> do
      cardCode <- asks (getId @CardCode eid)
      when (cardCode == "01116") $ do
        void popMessage
        unshiftMessage (EnemySpawn miid "01124" eid)
      YourHouse <$> runMessage msg attrs
    UseCardAbility iid source _ 1 | isSource attrs source ->
      l <$ unshiftMessages [DrawCards iid 1 False, TakeResources iid 1 False]
    _ -> YourHouse <$> runMessage msg attrs
