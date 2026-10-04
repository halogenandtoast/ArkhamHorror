module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Treacheries.KingTide (kingTide) where

import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers (
  canIncreaseFloodLevel,
  increaseThisFloodLevel,
 )
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as Cards
import Arkham.Investigator.Types (Field (InvestigatorKeys))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Import.Lifted

newtype KingTide = KingTide TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kingTide :: TreacheryCard KingTide
kingTide = treachery KingTide Cards.kingTide

instance RunMessage KingTide where
  runMessage msg t@(KingTide attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      -- "(unflooded if possible)" narrows the choice rather than widening it.
      unflooded <- select $ LocationWithInvestigator Anyone <> not_ FloodedLocation
      choices <- if null unflooded then select (LocationWithInvestigator Anyone) else pure unflooded
      chooseTargetM iid choices $ handleTarget iid attrs
      pure t
    HandleTargetChoice _ (isSource attrs -> True) (LocationTarget lid) -> do
      canIncrease <- canIncreaseFloodLevel lid
      -- "If the chosen location's flood level is not increased by this effect, King
      -- Tide gains surge." Increasing twice from unflooded reaches fully flooded.
      if canIncrease
        then replicateM_ 2 $ increaseThisFloodLevel lid
        else gainSurge attrs
      -- "An investigator at that location places one of their keys on that
      -- location": the lead names who, and that investigator picks the key.
      keyHolders <- filterM (fieldP InvestigatorKeys notNull) =<< select (investigatorAt lid)
      unless (null keyHolders) do
        lead <- getLead
        chooseOrRunOneM lead $ targets keyHolders \iid' -> do
          ks <- fieldMap InvestigatorKeys toList iid'
          chooseOneM iid' $ for_ ks \k -> keyLabeled k $ placeKey lid k
      pure t
    _ -> KingTide <$> liftRunMessage msg attrs
