module Arkham.Treachery.Cards.Replication (replication) where

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Types (Field (EnemyDamage, EnemyHealth, EnemyLocation))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.CreateEnemy
import Arkham.Projection
import Arkham.Tracing
import Arkham.Trait (Trait (Manifold))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Replication = Replication TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

replication :: TreacheryCard Replication
replication = treachery Replication Cards.replication

getMostDamagedManifold :: (HasGame m, Tracing m) => m (Maybe EnemyId)
getMostDamagedManifold = do
  manifolds <- selectWithField EnemyDamage $ EnemyWithTrait Manifold
  pure $ fst <$> headMay (sortOn (Down . snd) manifolds)

getEnemyPrintedHealth :: (HasGame m, Tracing m, CardGen m) => EnemyId -> m (Maybe Int)
getEnemyPrintedHealth eid = runMaybeT do
  card <- MaybeT $ fetchCardMaybe eid
  pc <- lift getPlayerCount
  hoistMaybe $ card.fixedHealth pc

instance RunMessage Replication where
  runMessage msg t@(Replication attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      getMostDamagedManifold >>= \case
        Nothing -> gainSurge attrs
        Just mostDamaged -> do
          pc <- getPlayerCount
          withinHealth <- maybe AnyCard (CardWithMaxPrintedHealth pc) <$> getEnemyPrintedHealth mostDamaged
          discardUntilFirst iid attrs Deck.EncounterDeck
            $ basic (#enemy <> CardWithTrait Manifold <> withinHealth)
      pure t
    RequestedEncounterCard (isSource attrs -> True) (Just iid) mcard -> do
      mMostDamaged <- getMostDamagedManifold
      case (mcard, mMostDamaged) of
        (Just card, Just mostDamaged) -> do
          field EnemyLocation mostDamaged >>= \case
            Nothing -> gainSurge attrs
            Just lid -> do
              runCreateEnemyT card lid \enemyId -> do
                setCreationInvestigator iid
                afterCreate $ handleTarget iid attrs enemyId
        _ -> gainSurge attrs
      pure t
    HandleTargetChoice iid (isSource attrs -> True) (EnemyTarget spawned) -> do
      mMostDamaged <- getMostDamagedManifold
      for_ mMostDamaged \mostDamaged -> when (mostDamaged /= spawned) do
        dmgA <- field EnemyDamage mostDamaged
        hA <- fromMaybe 0 <$> field EnemyHealth mostDamaged
        hB <- fromMaybe 0 <$> field EnemyHealth spawned
        let remainingA = max 0 (hA - dmgA)
        let remainingSum = remainingA + hB
        let moveable = min dmgA hB
        let toMoveFor desiredRemainingA = max 0 (min moveable (desiredRemainingA - remainingA))
        let toMoveHigh = toMoveFor ((remainingSum + 1) `div` 2)
        let toMoveLow = toMoveFor (remainingSum `div` 2)
        let
          move :: ReverseQueue m => Int -> m ()
          move toMove = when (toMove > 0) $ moveTokensNoDefeated attrs mostDamaged spawned #damage toMove
        if toMoveHigh == toMoveLow
          then move toMoveHigh
          else chooseOneM iid $ targets [mostDamaged, spawned] \chosen ->
            move $ if chosen == mostDamaged then toMoveHigh else toMoveLow
      pure t
    _ -> Replication <$> liftRunMessage msg attrs
