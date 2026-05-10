module Arkham.Act.Cards.TheDoomThatCameBefore (theDoomThatCameBefore) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Types
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheSearchForKadath.Helpers
import Arkham.Trait (Trait (Port))

newtype TheDoomThatCameBefore = TheDoomThatCameBefore ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theDoomThatCameBefore :: ActCard TheDoomThatCameBefore
theDoomThatCameBefore = act (2, A) TheDoomThatCameBefore Cards.theDoomThatCameBefore Nothing

instance HasAbilities TheDoomThatCameBefore where
  getAbilities (TheDoomThatCameBefore x) =
    [ restricted x 1 (EachUndefeatedInvestigator $ at_ $ LocationWithTrait Port)
        $ Objective
        $ freeReaction (RoundEnds #when)
    , restricted x 2 (HasScenarioCount SignOfTheGods $ atLeast 10)
        $ Objective
        $ forced AnyWindow
    ]

toOption :: (HasI18n, ReverseQueue m) => Region -> ChooseT m ()
toOption = \case
  Oriab ->
    labeled' "visitOriab"
      $ setScenarioMeta Oriab
  Mnar -> error "Not possible"
  ForbiddenLands ->
    labeled' "visitForbiddenLands"
      $ setScenarioMeta ForbiddenLands
  TimelessRealm ->
    labeled' "visitTimelessRealm"
      $ setScenarioMeta TimelessRealm

instance RunMessage TheDoomThatCameBefore where
  runMessage msg a@(TheDoomThatCameBefore attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      lead <- getLead
      selectEach (enemyIs Enemies.beingsOfIb <> not_ IsSwarm) removeEnemy
      findCard (`cardMatch` cardIs Enemies.beingsOfIb) >>= traverse_ obtainCard
      n <- scenarioFieldMap ScenarioMeta toResult
      let availableRegions = filter (`notElem` regions n) [Oriab, ForbiddenLands, TimelessRealm]
      if null availableRegions
        then push R1
        else chooseOrRunOneM lead $ scenarioI18n $ scope "theDoomThatCameBefore" $ traverse toOption availableRegions
      shuffleEncounterDiscardBackIn
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push R1
      pure a
    _ -> TheDoomThatCameBefore <$> liftRunMessage msg attrs
