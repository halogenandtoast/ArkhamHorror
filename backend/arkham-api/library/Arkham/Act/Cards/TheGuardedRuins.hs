module Arkham.Act.Cards.TheGuardedRuins (theGuardedRuins) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheGuardedRuins = TheGuardedRuins ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theGuardedRuins :: ActCard TheGuardedRuins
theGuardedRuins =
  act
    (3, A)
    TheGuardedRuins
    Cards.theGuardedRuins
    (Just $ GroupClueCost (PerPlayer 2) (LocationWithTitle "Ruins of Eztli"))

instance HasModifiersFor TheGuardedRuins where
  getModifiersFor (TheGuardedRuins a) = do
    modifySelect a (EnemyWithTitle "Eztli Guardian") [EnemyFight 1, EnemyEvade 1]
    validCards <- findAllCards (`isCard` Treacheries.arrowsFromTheTrees)
    modifyEach a validCards [AddKeyword Keyword.Surge]

instance RunMessage TheGuardedRuins where
  runMessage msg a@(TheGuardedRuins attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R2
      pure a
    _ -> TheGuardedRuins <$> liftRunMessage msg attrs
