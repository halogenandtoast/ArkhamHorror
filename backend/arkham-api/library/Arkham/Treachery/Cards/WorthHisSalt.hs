module Arkham.Treachery.Cards.WorthHisSalt (worthHisSalt, WorthHisSalt (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Modifier
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (EnemyAttacks)

newtype WorthHisSalt = WorthHisSalt TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

worthHisSalt :: TreacheryCard WorthHisSalt
worthHisSalt = treachery WorthHisSalt Cards.worthHisSalt

instance HasAbilities WorthHisSalt where
  getAbilities (WorthHisSalt a) = case a.placement of
    AttachedToEnemy eid ->
      [ groupLimit PerRound
          $ mkAbility a 1
          $ forced
          $ MovedFromHunter #after (EnemyWithId eid <> UnengagedEnemy)
      , mkAbility a 2 $ forced $ EnemyAttacks #when Anyone AnyEnemyAttack (EnemyWithId eid)
      ]
    _ -> []

instance RunMessage WorthHisSalt where
  runMessage msg t@(WorthHisSalt attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      mOceirosMarsh <- selectOne $ IncludeOutOfPlayEnemy $ enemyIs Enemies.oceirosMarsh
      case mOceirosMarsh of
        Just oceirosMarsh -> place attrs (AttachedToEnemy oceirosMarsh)
        Nothing -> do
          mcard <- select $ VictoryDisplayCardMatch $ basic $ cardIs Enemies.oceirosMarsh
          for_ mcard \card -> do
            oceirosMarsh <- createEnemy card (OutOfPlay VictoryDisplayZone)
            place attrs (AttachedToEnemy oceirosMarsh)
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      case attrs.placement of
        AttachedToEnemy eid -> do
          isEngaged <- selectAny $ investigatorEngagedWith eid
          unless isEngaged do
            phaseModifier (attrs.ability 1) eid CannotAttack
            push $ HunterMove eid
        _ -> pure ()
      pure t
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      case attrs.placement of
        AttachedToEnemy eid -> do
          enemyAttackModifiers (attrs.ability 2) eid [DamageDealt 1, HorrorDealt 1]
          toDiscard (attrs.ability 1) attrs
        _ -> pure ()
      pure t
    _ -> WorthHisSalt <$> liftRunMessage msg attrs
