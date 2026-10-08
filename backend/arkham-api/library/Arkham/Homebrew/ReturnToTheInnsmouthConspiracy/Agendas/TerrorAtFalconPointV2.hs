module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Agendas.TerrorAtFalconPointV2 (
  terrorAtFalconPointV2,
) where

import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.ALightInTheFog qualified as Enemies
import Arkham.Helpers.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Enemies qualified as HBEnemies
import Arkham.Location.Grid
import Arkham.Location.Types (Field (LocationPosition))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Scenarios.TheInnsmouthConspiracy.ALightInTheFog.Helpers

newtype TerrorAtFalconPointV2 = TerrorAtFalconPointV2 AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

terrorAtFalconPointV2 :: AgendaCard TerrorAtFalconPointV2
terrorAtFalconPointV2 =
  agenda (4, A) TerrorAtFalconPointV2 Cards.terrorAtFalconPointV2 (Static 3)

{- | As the official agenda, plus "Locations on the same row are connected to one
another". Rows come from the grid, so the connections are emitted per location rather
than declared on the cards.
-}
instance HasModifiersFor TerrorAtFalconPointV2 where
  getModifiersFor (TerrorAtFalconPointV2 a) = do
    healthModifier <- perPlayer 2
    modifySelect a (enemyIs Enemies.oceirosMarsh) [HealthModifier healthModifier]
    positioned <- select Anywhere >>= traverse (\lid -> (lid,) <$> field LocationPosition lid)
    let rowOf y = [lid | (lid, Just (Pos _ y')) <- positioned, y' == y]
    for_ positioned \case
      (lid, Just (Pos _ y)) -> do
        let others = filter (/= lid) (rowOf y)
        unless (null others) do
          modifySelect
            a
            (LocationWithId lid)
            [ConnectedToWhen (LocationWithId lid) (mapOneOf LocationWithId others)]
      _ -> pure ()

instance RunMessage TerrorAtFalconPointV2 where
  runMessage msg a@(TerrorAtFalconPointV2 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      could <- floodBottommost 4
      if could
        then do
          grapplers <- getSetAsideCardsMatching (cardIs HBEnemies.deepOneGrappler)
          unless (null grapplers) do
            spawnEnemy_ HBEnemies.deepOneGrappler
          push $ RevertAgenda attrs.id
        else push R3
      pure a
    _ -> TerrorAtFalconPointV2 <$> liftRunMessage msg attrs
