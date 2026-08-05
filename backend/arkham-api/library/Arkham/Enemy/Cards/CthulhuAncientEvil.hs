module Arkham.Enemy.Cards.CthulhuAncientEvil (cthulhuAncientEvil) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (EnemyHealthDamage, EnemySanityDamage))
import Arkham.Helpers.Modifiers (ModifierType (..), modifyEach, modifySelf)
import Arkham.Keyword qualified as Keyword
import Arkham.Placement
import Arkham.Projection
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers (
  cthulhuPatrolledMarker,
  getCthulhuBoardEnemies,
 )
import Arkham.Trait (Trait (AncientOne))

newtype CthulhuAncientEvil = CthulhuAncientEvil EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

cthulhuAncientEvil :: EnemyCard CthulhuAncientEvil
cthulhuAncientEvil = enemy CthulhuAncientEvil Cards.cthulhuAncientEvil

instance HasModifiersFor CthulhuAncientEvil where
  getModifiersFor (CthulhuAncientEvil a) = do
    modifySelf
      a
      [ CannotMakeAttacksOfOpportunity
      , CannotBeAttacked
      , CannotBeEvaded
      , CannotBeDamaged
      ]

    facets <- getCthulhuBoardEnemies
    damage <- sum <$> traverse (field EnemyHealthDamage) facets
    horror <- sum <$> traverse (field EnemySanityDamage) facets
    modifySelf a [DamageDealt damage, HorrorDealt horror]

    modifyEach
      a
      facets
      [ CannotAttackDuringEnemyPhase
      , CannotMakeAttacksOfOpportunity
      , AddKeyword Keyword.Massive
      , AddTrait AncientOne
      ]

instance RunMessage CthulhuAncientEvil where
  runMessage msg (CthulhuAncientEvil attrs) = runQueueT $ case msg of
    EnemyMove eid lid | eid == attrs.id -> do
      facets <- getCthulhuBoardEnemies
      for_ facets \facet -> push $ PlaceEnemy facet (AtLocation lid)
      CthulhuAncientEvil <$> liftRunMessage msg attrs
    {- Dire Gale asks whether Cthulhu "did not move via his patrol keyword this
    round", and enemy movement is not recorded in the history, so the patrol marks
    itself with a round-scoped modifier that expires when the round does. -}
    PatrolMove eid _ | eid == attrs.id -> do
      roundModifier attrs attrs cthulhuPatrolledMarker
      CthulhuAncientEvil <$> liftRunMessage msg attrs
    _ -> CthulhuAncientEvil <$> liftRunMessage msg attrs
