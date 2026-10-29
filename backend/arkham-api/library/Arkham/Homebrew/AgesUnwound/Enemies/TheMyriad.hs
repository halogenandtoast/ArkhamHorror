module Arkham.Homebrew.AgesUnwound.Enemies.TheMyriad (theMyriad) where

import Arkham.Ability
import Arkham.Attack.Types (attackEnemy)
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (EnemyLocation, EnemyMeta, EnemyPlacement, EnemyWantsToAttack))
import Arkham.Helpers.Query (getLead)
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Projection
import Arkham.Window qualified as Window

newtype TheMyriad = TheMyriad EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMyriad :: EnemyCard TheMyriad
theMyriad = enemyWith TheMyriad Cards.theMyriad (spawnAtL ?~ SpawnAt (locationIs Locations.thePresent))

instance HasAbilities TheMyriad where
  getAbilities (TheMyriad a) =
    extend a case a.placement of
      AsSwarm {} -> []
      _ ->
        [ restricted a 1 (DuringPhase #enemy <> youExist LeadInvestigator)
            $ (if fst (myriadMeta a.meta) then silent else forced)
            $ EnemyAttacksWithAnyTarget #when AnyEnemyAttack (oneOf [be a, SwarmOf a.id])
        ]

-- The host owns both the activation flag and the allocation record. Accept the
-- old allocation-only metadata so games saved during an enemy phase keep it.
myriadMeta :: Value -> (Bool, [InvestigatorId])
myriadMeta value = fromMaybe (False, toResultDefault [] value) (maybeResult value)

instance RunMessage TheMyriad where
  runMessage msg (TheMyriad attrs) = runQueueT $ case msg of
    FinishedEnemyAttacks batchEnemies
      | attrs.id `elem` batchEnemies ->
          pure $ TheMyriad $ setMeta (False, [] :: [InvestigatorId]) attrs
    UseCardAbility _ (isSource attrs -> True) 1 windows _ -> do
      -- The first attacker can be a swarm card; the host handles every member's
      -- window, switching the same ability to silent for subsequent attacks.
      let attackingEnemyIds = nub [attackEnemy details | (Window.windowType -> Window.EnemyAttacks details) <- windows]
      for_ attackingEnemyIds \attacker -> do
        mlid <- field EnemyLocation attacker
        for_ mlid \lid -> do
          iids <- select $ investigatorAt lid
          -- Include hosts and swarm members when finding the groups at this
          -- location, then read each host's record only once.
          copies <- select $ enemyIs Cards.theMyriad <> EnemyAt (LocationWithId lid)
          hosts <-
            nub <$> for copies \eid -> do
              placement <- field EnemyPlacement eid
              pure $ case placement of
                AsSwarm host _ -> host
                _ -> eid
          assigned <- concatMapM (fieldMap EnemyMeta (snd . myriadMeta)) hosts
          pending <- length <$> filterM (field EnemyWantsToAttack) (filter (/= attacker) copies)
          let counts = [(iid, count (== iid) (assigned :: [InvestigatorId])) | iid <- iids]
          unless (null counts) do
            let
              total = sum (map snd counts) + pending + 1
              lower = total `div` length counts
              upper = (total + length counts - 1) `div` length counts
              canAssign chosen =
                let after = [n + if iid == chosen then 1 else 0 | (iid, n) <- counts]
                 in all (<= upper) after && sum [max 0 (lower - n) | n <- after] <= pending
            lead <- getLead
            chooseOrRunOneM lead do
              for_ iids \iid -> when (canAssign iid) do
                targeting iid do
                  push $ ChangeEnemyAttackTarget attacker (toTarget iid)
                  push $ ScenarioSpecific "myriadAttackAssigned" $ toJSON (attrs.id, iid)
      pure $ TheMyriad $ setMeta (True, snd $ myriadMeta attrs.meta) attrs
    ScenarioSpecific "myriadAttackAssigned" value
      | Just (host, iid) <- maybeResult @(EnemyId, InvestigatorId) value
      , host == attrs.id ->
          pure $ TheMyriad $ setMeta (fst $ myriadMeta attrs.meta, snd (myriadMeta attrs.meta) <> [iid]) attrs
    _ -> TheMyriad <$> liftRunMessage msg attrs
