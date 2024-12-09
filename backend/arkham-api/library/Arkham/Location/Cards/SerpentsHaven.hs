module Arkham.Location.Cards.SerpentsHaven (serpentsHaven, SerpentsHaven (..)) where

import Arkham.Ability
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Treacheries

newtype SerpentsHaven = SerpentsHaven LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

serpentsHaven :: LocationCard SerpentsHaven
serpentsHaven = location SerpentsHaven Cards.serpentsHaven 2 (PerPlayer 2)

instance HasModifiersFor SerpentsHaven where
  getModifiersFor (SerpentsHaven a) = do
    modifySelect a (enemyAt a <> EnemyWithTrait Serpent) [EnemyFight 1]

-- NOTE: Because an explore will move you to Serpent's Haven, it will trigger
-- the forced ability even if you started the action somewhere else. In order
-- to fix this we use the meta to track that you started the action at
-- Serpent's Haven.
instance HasAbilities SerpentsHaven where
  getAbilities (SerpentsHaven attrs) =
    extendRevealed
      attrs
      [ restrictedAbility attrs 0 activeCriteria $ silent (trigger #when)
      , restrictedAbility attrs 1 criteria $ forced (trigger #after)
      ]
   where
    trigger timing = PerformAction timing You (oneOf [#investigate, #explore])
    activeCriteria = Here <> exists (treacheryIs Treacheries.poisoned <> TreacheryInThreatAreaOf You)
    criteria =
      if getLocationMetaDefault False attrs
        then activeCriteria
        else Never

instance RunMessage SerpentsHaven where
  runMessage msg l@(SerpentsHaven attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 0 -> do
      pure $ SerpentsHaven $ attrs & setMeta True
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure l
    FinishAction -> do
      pure $ SerpentsHaven $ attrs & setMeta False
    _ -> SerpentsHaven <$> liftRunMessage msg attrs
