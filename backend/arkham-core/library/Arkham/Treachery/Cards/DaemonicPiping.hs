module Arkham.Treachery.Cards.DaemonicPiping (daemonicPiping, DaemonicPiping (..)) where

import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Card
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DaemonicPiping = DaemonicPiping TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

daemonicPiping :: TreacheryCard DaemonicPiping
daemonicPiping = treachery DaemonicPiping Cards.daemonicPiping

instance RunMessage DaemonicPiping where
  runMessage msg t@(DaemonicPiping attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      selectOne (enemyIs Enemies.piperOfAzathoth) >>= \case
        Just piper -> do
          selectEach
            (InvestigatorAt $ oneOf [locationWithEnemy piper, ConnectedTo (locationWithEnemy piper)])
            \investigator -> assignHorror investigator attrs 1
        Nothing -> placeTreachery attrs NextToAgenda
      pure t
    AfterRevelation _ tid | tid == toId attrs -> do
      daemonicPipings <- select $ treacheryIs Cards.daemonicPiping
      when (length daemonicPipings >= 3) $ do
        piperOfAzathoth <- findJustCard (`cardMatch` cardIs Enemies.piperOfAzathoth)
        for_ daemonicPipings $ toDiscard attrs
        createEnemyEngagedWithPrey_ piperOfAzathoth
      pure t
    _ -> DaemonicPiping <$> liftRunMessage msg attrs
