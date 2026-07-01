module Arkham.Treachery.Cards.WhileTheySleep (whileTheySleep) where

import Arkham.Matcher
import Arkham.Scenarios.WarOfTheOuterGods.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WhileTheySleep = WhileTheySleep TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whileTheySleep :: TreacheryCard WhileTheySleep
whileTheySleep = treachery WhileTheySleep Cards.whileTheySleep

instance RunMessage WhileTheySleep where
  runMessage msg t@(WhileTheySleep attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      counts <- for factionOrder \f -> (f,) <$> selectCount (InPlayEnemy $ factionEnemy f)
      let highest = maximumEx $ map snd counts
      if highest == 0
        then gainSurge attrs
        else for_ [f | (f, n) <- counts, n == highest] \f ->
          placeDoomOnFactionAgenda attrs f 1
      pure t
    _ -> WhileTheySleep <$> liftRunMessage msg attrs
