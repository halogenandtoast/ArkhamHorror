module Arkham.Treachery.Cards.DayOfReckoning (dayOfReckoning, DayOfReckoning (..)) where

import Arkham.Card
import Arkham.Matcher
import Arkham.Message (pattern AttachTreachery)
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DayOfReckoning = DayOfReckoning TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dayOfReckoning :: TreacheryCard DayOfReckoning
dayOfReckoning = treachery DayOfReckoning Cards.dayOfReckoning

instance RunMessage DayOfReckoning where
  runMessage msg t@(DayOfReckoning attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      agendas <- select AnyAgenda
      when (notNull agendas) $ do
        chooseOrRunOne
          iid
          [targetLabel agenda [AttachTreachery (toId attrs) (toTarget agenda)] | agenda <- agendas]
        mElderSign <- selectOne $ ChaosTokenFaceIs #eldersign
        for_ mElderSign $ \token -> do
          pushAll
            $ [ SealChaosToken token
              , SealedChaosToken token (toCard attrs)
              ]
      pure t
    _ -> DayOfReckoning <$> lift (runMessage msg attrs)
