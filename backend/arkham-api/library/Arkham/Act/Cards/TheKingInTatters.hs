module Arkham.Act.Cards.TheKingInTatters (theKingInTatters) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Location
import Arkham.Matcher

newtype TheKingInTatters = TheKingInTatters ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theKingInTatters :: ActCard TheKingInTatters
theKingInTatters = act (3, A) TheKingInTatters Cards.theKingInTatters Nothing

instance HasAbilities TheKingInTatters where
  getAbilities (TheKingInTatters a) =
    [ restricted a 1 (OnLocation $ LocationWithoutClues <> LocationCanBeFlipped) $ FastAbility Free
    , mkAbility a 2 $ forced $ EnemyWouldBeDefeated #when $ EnemyWithTitle "Hastur"
    ]

instance RunMessage TheKingInTatters where
  runMessage msg a@(TheKingInTatters attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \lid -> do
        whenMatch lid LocationWithoutClues $ eachInvestigator $ discardAllClues (attrs.ability 1)
        flipOverBy iid (attrs.ability 1) lid
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      whenM (selectAny $ enemyIs Enemies.hasturTheTatteredKing) $ push R1
      whenM (selectAny $ enemyIs Enemies.hasturTheKingInYellow) $ push R2
      whenM (selectAny $ enemyIs Enemies.hasturLordOfCarcosa) $ push R3
      pure a
    _ -> TheKingInTatters <$> liftRunMessage msg attrs
