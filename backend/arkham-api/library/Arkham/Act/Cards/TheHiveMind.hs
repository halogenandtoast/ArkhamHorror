module Arkham.Act.Cards.TheHiveMind (theHiveMind) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Investigator (getSpendableClueCount)
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheApiary.Helpers (scenarioI18n)

newtype TheHiveMind = TheHiveMind ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHiveMind :: ActCard TheHiveMind
theHiveMind = act (2, A) TheHiveMind Cards.theHiveMind Nothing

instance HasAbilities TheHiveMind where
  getAbilities (TheHiveMind a) =
    extend
      a
      [ restricted a 1 (DuringTurn You <> exists (EnemyAt YourLocation <> NonEliteEnemy))
          $ FastAbility Free
      , mkAbility a 2 $ forced $ RoundEnds #when
      , mkAbility a 3 $ Objective $ forced $ IfEnemyDefeated #after Anyone ByAny (enemyIs Enemies.mother)
      ]

instance RunMessage TheHiveMind where
  runMessage msg a@(TheHiveMind attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- getSpendableClueCount iid
      when (n > 0) $ scenarioI18n $ chooseAmount' iid "cluesToSpend" "$clues" 0 n attrs
      pure a
    ResolveAmounts iid (getChoiceAmount "$clues" -> n) (isTarget attrs -> True) | n > 0 -> do
      enemies <- select $ EnemyAt (locationWithInvestigator iid) <> NonEliteEnemy
      chooseTargetM iid enemies \enemy -> do
        placeCluesOnLocation iid (attrs.ability 1) n
        nonAttackEnemyDamage (Just iid) (attrs.ability 1) n enemy
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      heads <- sample2 True False
      scenarioSpecific
        "rotateCentralChamber"
        ((if heads then "clockwise" else "counterclockwise") :: Text)
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      push R3
      pure a
    _ -> TheHiveMind <$> liftRunMessage msg attrs
