module Arkham.Treachery.Cards.Replication (replication) where

import Arkham.Capability
import Arkham.Classes.HasGame
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Types (Field (EnemyDamage, EnemyHealth, EnemyLocation))
import Arkham.Helpers.Scenario (getEncounterDeckKey)
import Arkham.Matcher
import Arkham.Message.Lifted.CreateEnemy
import Arkham.Projection
import Arkham.Trait (Trait (Manifold))
import Arkham.Tracing
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Replication = Replication TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

replication :: TreacheryCard Replication
replication = treachery Replication Cards.replication

-- | Returns the Manifold enemy in play with the most damage on it (if any).
getMostDamagedManifold :: (HasGame m, Tracing m) => m (Maybe EnemyId)
getMostDamagedManifold = do
  manifolds <- selectWithField EnemyDamage $ EnemyWithTrait Manifold
  pure $ case sortOn snd manifolds of
    [] -> Nothing
    xs -> Just $ fst $ last xs

instance RunMessage Replication where
  runMessage msg t@(Replication attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      mMostDamaged <- getMostDamagedManifold
      case mMostDamaged of
        Nothing -> gainSurge attrs
        Just _ -> do
          hasEncounterDeck <- can.target.encounterDeck iid
          if hasEncounterDeck
            then do
              key <- getEncounterDeckKey iid
              -- NOTE: We cannot express "Manifold enemy with printed health <=
              -- the most-damaged enemy" in the card matcher, so we discard until
              -- the FIRST Manifold enemy and validate the printed-health
              -- constraint when it is found (see RequestedEncounterCard).
              push
                $ DiscardUntilFirst
                  iid
                  (toSource attrs)
                  (Deck.EncounterDeckByKey key)
                  (BasicCardMatch $ #enemy <> CardWithTrait Manifold)
            else gainSurge attrs
      pure t
    RequestedEncounterCard (isSource attrs -> True) (Just iid) mcard -> do
      mMostDamaged <- getMostDamagedManifold
      case (mcard, mMostDamaged) of
        (Just card, Just mostDamaged) -> do
          mlid <- field EnemyLocation mostDamaged
          case mlid of
            Nothing -> gainSurge attrs
            Just lid -> do
              -- Spawn the discarded enemy at the same location as the
              -- most-damaged Manifold enemy. After it is created, redistribute
              -- damage between the two (see HandleTargetChoice).
              --
              -- NOTE: The printed-health (<=) constraint from the card text is
              -- NOT enforced; we spawn the first Manifold enemy discarded
              -- regardless of its printed health (the matcher cannot express
              -- "printed health <= X").
              runCreateEnemyT card lid \enemyId -> do
                setCreationInvestigator iid
                afterCreate
                  $ push
                  $ HandleTargetChoice iid (toSource attrs) (EnemyTarget enemyId)
        _ -> gainSurge attrs
      pure t
    HandleTargetChoice _ (isSource attrs -> True) (EnemyTarget spawned) -> do
      -- Redistribute damage between the most-damaged Manifold enemy and the
      -- newly spawned one so that remaining health on both is as equal as
      -- possible. The spawned enemy starts with 0 damage, so total damage is
      -- conserved on the most-damaged enemy.
      mMostDamaged <- getMostDamagedManifold
      for_ mMostDamaged \mostDamaged -> when (mostDamaged /= spawned) do
        dmgA <- field EnemyDamage mostDamaged
        hA <- fromMaybe 0 <$> field EnemyHealth mostDamaged
        hB <- fromMaybe 0 <$> field EnemyHealth spawned
        let total = dmgA -- spawned enemy starts undamaged
        -- remaining-health sum is fixed = (hA + hB) - total. Split it so the
        -- two remaining-health values are as equal as possible.
        let remainingSum = (hA + hB) - total
        let targetRemainingA = max 0 (min hA ((remainingSum + 1) `div` 2))
        let dmgA' = max 0 (min hA (hA - targetRemainingA))
        let toMove = max 0 (dmgA - dmgA')
        when (toMove > 0)
          $ moveTokensNoDefeated attrs mostDamaged spawned #damage toMove
      pure t
    _ -> Replication <$> liftRunMessage msg attrs
