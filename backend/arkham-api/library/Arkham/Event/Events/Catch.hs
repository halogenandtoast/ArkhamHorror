module Arkham.Event.Events.Catch (catch) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted hiding (catch)
import Arkham.Helpers.SkillTest (getSkillTestTargetedEnemy)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Zone

newtype Catch = Catch EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

catch :: EventCard Catch
catch = event Catch Cards.catch

instance RunMessage Catch where
  runMessage msg e@(Catch attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      case attrs.payment.discards of
        [(_zone, discardedCard)] -> do
          let n = discardedCard.printedCost
          sid <- getRandom
          when (n > 0) $ skillTestModifier sid attrs iid $ SkillModifier #agility n
          chooseEvadeEnemy sid iid attrs
        _ -> error $ "Invalid choice: " <> show attrs.payment
      pure e
    PassedThisSkillTest _iid (isSource attrs -> True) -> do
      case attrs.payment.discards of
        [(zone, _discardedCard)] -> do
          when (zone == FromPlay) do
            getSkillTestTargetedEnemy >>= traverse_ \enemy -> do
              unlessMatch enemy EliteEnemy
                $ nextPhaseModifier #upkeep attrs enemy DoesNotReadyDuringUpkeep
        _ -> pure ()
      pure e
    _ -> Catch <$> liftRunMessage msg attrs
