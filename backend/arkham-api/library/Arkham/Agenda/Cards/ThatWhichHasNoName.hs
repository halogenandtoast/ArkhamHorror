module Arkham.Agenda.Cards.ThatWhichHasNoName (thatWhichHasNoName) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Window (getBatchId)

newtype ThatWhichHasNoName = ThatWhichHasNoName AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thatWhichHasNoName :: AgendaCard ThatWhichHasNoName
thatWhichHasNoName = agenda (2, A) ThatWhichHasNoName Cards.thatWhichHasNoName (Static 12)

instance HasAbilities ThatWhichHasNoName where
  getAbilities (ThatWhichHasNoName a) =
    [ restricted a 1 (SetAsideCardExists $ cardIs Enemies.theNamelessMadness)
        $ forced
        $ WouldPlaceDoomCounter #when #any #any
    , restricted a 2 (EnemyCount 15 $ enemyIs Enemies.theNamelessMadness) $ forced $ RoundEnds #when
    ]

instance RunMessage ThatWhichHasNoName where
  runMessage msg a@(ThatWhichHasNoName attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    UseCardAbility iid (isSource attrs -> True) 1 (getBatchId -> batchId) _ -> do
      push $ IgnoreBatch batchId
      ls <-
        select
          $ NearestLocationToAny
          $ not_ (LocationWithEnemy $ enemyIs Enemies.theNamelessMadness)
          <> ConnectedTo (LocationWithEnemy $ enemyIs Enemies.theNamelessMadness)
      chooseTargetM iid ls $ createSetAsideEnemy_ Enemies.theNamelessMadness
      pure a
    _ -> ThatWhichHasNoName <$> liftRunMessage msg attrs
