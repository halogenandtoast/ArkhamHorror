module Arkham.Treachery.Cards.DayOfReckoning (dayOfReckoning, DayOfReckoning (..)) where

import Arkham.Card
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers qualified as Msg
import Arkham.Treachery.Import.Lifted

newtype DayOfReckoning = DayOfReckoning TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dayOfReckoning :: TreacheryCard DayOfReckoning
dayOfReckoning = treachery DayOfReckoning Cards.dayOfReckoning

instance RunMessage DayOfReckoning where
  runMessage msg t@(DayOfReckoning attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      selectWhenNotNull AnyAgenda \agendas -> do
        chooseOrRunOne iid $ targetLabels agendas $ only . Msg.attachTreachery attrs
        selectOne (ChaosTokenFaceIs #eldersign) >>= traverse_ \token -> do
          pushAll [SealChaosToken token, SealedChaosToken token (toCard attrs)]
      pure t
    _ -> DayOfReckoning <$> liftRunMessage msg attrs
