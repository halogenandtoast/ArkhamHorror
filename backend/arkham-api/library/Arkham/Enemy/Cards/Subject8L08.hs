module Arkham.Enemy.Cards.Subject8L08 (subject8L08) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Modifier (UIModifier (Oversized))

newtype Subject8L08 = Subject8L08 EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor Subject8L08 where
  getModifiersFor (Subject8L08 a) = modifySelf a [UIModifier Oversized, HealthModifier 15]

instance HasAbilities Subject8L08 where
  getAbilities (Subject8L08 a) = [mkAbility a 1 $ Objective $ forced $ EnemyDefeated #when Anyone ByAny (be a)]

subject8L08 :: EnemyCard Subject8L08
subject8L08 =
  enemyWith Subject8L08 Cards.subject8L08
    $ \a -> a {enemyFight = Nothing, enemyEvade = Nothing}

-- Subject 8L-08 devours the card behind the given reference: the entity (if any)
-- is removed from play, the card is pulled from wherever it is, and placed
-- beneath the subject (out of play, returned to its owner at the end of the
-- game). `remove` is the message that takes the entity off the table; for a card
-- that is not in play (in a deck/hand/discard) it is a no-op.
devourRef :: (ReverseQueue m, FetchCard ref) => EnemyAttrs -> m () -> ref -> m ()
devourRef attrs remove ref = do
  card <- fetchCard ref
  remove
  obtainCard card
  placeUnderneath attrs [card]

instance RunMessage Subject8L08 where
  runMessage msg e@(Subject8L08 attrs) = runQueueT $ case msg of
    ScenarioSpecific "devour" (maybeResult -> Just target) -> do
      case target of
        LocationTarget lid -> devourRef attrs (removeLocation lid) lid
        AssetTarget aid -> devourRef attrs (removeAsset aid) aid
        EnemyTarget eid -> devourRef attrs (removeEnemy eid) eid
        TreacheryTarget tid -> devourRef attrs (removeTreachery tid) tid
        CardIdTarget cid -> devourRef attrs (pure ()) cid
        _ -> pure ()
      pure e
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      push R2
      pure e
    _ -> Subject8L08 <$> liftRunMessage msg attrs
