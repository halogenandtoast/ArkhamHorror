module Arkham.Act.Cards.TheDreamEaters.WhereTheGodsDwell.JourneyThroughTheColdWastes (
  JourneyThroughTheColdWastes (..),
  journeyThroughTheColdWastes,
) where

import Arkham.Act.CardDefs.TheDreamEaters.WhereTheGodsDwell qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.CardDefs.TheDreamEaters.WhereTheGodsDwell qualified as Enemies
import Arkham.Location.CardDefs.TheDreamEaters.WhereTheGodsDwell qualified as Locations
import Arkham.Matcher

newtype JourneyThroughTheColdWastes = JourneyThroughTheColdWastes ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

journeyThroughTheColdWastes :: ActCard JourneyThroughTheColdWastes
journeyThroughTheColdWastes =
  act
    (1, A)
    JourneyThroughTheColdWastes
    Cards.journeyThroughTheColdWastes
    (groupClueCost $ PerPlayer 2)

instance RunMessage JourneyThroughTheColdWastes where
  runMessage msg a@(JourneyThroughTheColdWastes attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      monastaryOfLeng <- selectJust $ locationIs Locations.monasteryOfLeng
      reveal monastaryOfLeng
      highPriestNotToBeDescribed <- getSetAsideCard Enemies.highPriestNotToBeDescribed
      createEnemyAt_ highPriestNotToBeDescribed monastaryOfLeng
      advanceActDeck attrs
      pure a
    _ -> JourneyThroughTheColdWastes <$> liftRunMessage msg attrs
