module Arkham.Event.Events.ButterflyEffect1 (butterflyEffect1, ButterflyEffect1 (..)) where

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getCommittableCards, getCommittedCards, getSkillTestInvestigator)
import Arkham.Tracing

newtype ButterflyEffect1 = ButterflyEffect1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

butterflyEffect1 :: EventCard ButterflyEffect1
butterflyEffect1 = event ButterflyEffect1 Cards.butterflyEffect1

{- | The cards an investigator could commit, and the cards they have committed
that this event is allowed to return to their hand.

'MustBeCommitted' is not a disqualifier here: it only means the investigator
may not take back the commit decision during the commit window (see
@uncommittableCards@ in "Arkham.Investigator.Runner"). Returning a committed
card to hand via a card effect is a different thing entirely. Only cards that
never left the zone they were committed from (Amanda Sharpe's top card,
Dayana Esperence's stashed cards) cannot be returned.
-}
butterflyEffectOptions :: (HasGame m, Tracing m) => InvestigatorId -> m ([Card], [Card])
butterflyEffectOptions who = do
  committable <- getCommittableCards who
  returnable <- filterM (`withoutModifier` LeaveCardWhereItIs) =<< getCommittedCards who
  pure (committable, returnable)

butterflyEffectChoice :: ReverseQueue m => InvestigatorId -> ([Card], [Card]) -> m ()
butterflyEffectChoice who (committable, returnable) = chooseOneM who do
  targets committable $ push . CommitCard who
  targets returnable \card -> push $ ReturnToHand who (CardIdTarget card.id)
  labeledI "doNothing" nothing

instance RunMessage ButterflyEffect1 where
  runMessage msg e@(ButterflyEffect1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      performer <- fromJustNote "must be in skill test" <$> getSkillTestInvestigator

      -- "You or the performing investigator may either commit a card to this
      -- skill test, or return a card they committed to this test to their
      -- hand." Only one of the two acts, so we ask who first, but skip that
      -- question when only one of them can do anything.
      candidates <- forMaybeM (if performer == iid then [iid] else [iid, performer]) \who -> do
        options@(committable, returnable) <- butterflyEffectOptions who
        pure $ guard (notNull committable || notNull returnable) $> (who, options)

      case candidates of
        [] -> pure ()
        [(who, options)] -> butterflyEffectChoice who options
        _ -> chooseOneM iid $ for_ candidates \(who, options) ->
          targeting who $ butterflyEffectChoice who options

      pure e
    _ -> ButterflyEffect1 <$> liftRunMessage msg attrs
