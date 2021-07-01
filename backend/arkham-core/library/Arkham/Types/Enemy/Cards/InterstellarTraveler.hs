module Arkham.Types.Enemy.Cards.InterstellarTraveler
  ( interstellarTraveler
  , InterstellarTraveler(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Trait

newtype InterstellarTraveler = InterstellarTraveler EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

interstellarTraveler :: EnemyCard InterstellarTraveler
interstellarTraveler = enemy InterstellarTraveler Cards.interstellarTraveler
  $ (healthDamageL .~ 1)
  . (sanityDamageL .~ 2)
  . (fightL .~ 4)
  . (healthL .~ Static 3)
  . (evadeL .~ 2)
  . (spawnAtL ?~ LocationWithTrait Extradimensional)

instance HasModifiersFor env InterstellarTraveler where
  getModifiersFor = noModifiersFor

instance EnemyAttrsHasActions env => HasActions env InterstellarTraveler where
  getActions i window (InterstellarTraveler attrs) = getActions i window attrs

instance (HasCount ClueCount env LocationId, EnemyAttrsRunMessage env) => RunMessage env InterstellarTraveler where
  runMessage msg (InterstellarTraveler attrs) = case msg of
    EnemyEntered eid lid | eid == enemyId attrs -> do
      clueCount <- unClueCount <$> getCount lid
      when (clueCount > 0) (unshiftMessage $ RemoveClues (LocationTarget lid) 1)
      pure . InterstellarTraveler $ attrs & doomL +~ 1
    _ -> InterstellarTraveler <$> runMessage msg attrs
