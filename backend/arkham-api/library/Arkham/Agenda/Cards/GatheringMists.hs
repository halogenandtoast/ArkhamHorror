module Arkham.Agenda.Cards.GatheringMists (gatheringMists) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Scenario (scenarioFieldMap)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (InvestigatorDefeated)
import Arkham.Matcher qualified as Matcher
import Arkham.Projection
import Arkham.Scenario.Types (Field (..))
import Arkham.Scenarios.LaidToRest.Helpers
import Arkham.Token qualified as Token
import Arkham.Trait (Trait (Spectral))

newtype GatheringMists = GatheringMists AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gatheringMists :: AgendaCard GatheringMists
gatheringMists = agenda (1, A) GatheringMists Cards.gatheringMists (Static 4)

instance HasAbilities GatheringMists where
  getAbilities (GatheringMists a) =
    [ restricted
        a
        1
        ( exists
            $ enemyIs Enemies.ravenousSpirit
            <> ReadyEnemy
            <> EnemyAt (LocationWithEnemy $ EnemyWithTitle "Jean Devereux")
        )
        $ forced
        $ PhaseBegins #when #enemy
    , mkAbility a 2 $ forced $ Matcher.InvestigatorDefeated #when ByAny jimCulver
    , scenarioI18n
        $ withI18nTooltip "gatheringMists.parley"
        $ restricted a 3 (exists theBeyond)
        $ parleyAction (GroupClueCost (PerPlayer 2) Anywhere)
    ]

instance RunMessage GatheringMists where
  runMessage msg a@(GatheringMists attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      jean <- selectJust $ EnemyWithTitle "Jean Devereux"
      n <-
        selectCount
          $ enemyIs Enemies.ravenousSpirit
          <> ReadyEnemy
          <> EnemyAt (LocationWithEnemy $ EnemyWithTitle "Jean Devereux")
      nonAttackEnemyDamage Nothing (attrs.ability 1) n jean
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      actId <- selectJust AnyAct
      push $ AdvanceAct actId (toSource attrs) #other
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      banishTopOfSpiritDeck
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      placeTokens attrs ScenarioTarget #horror 1
      doStep 1 msg
      pure a
    DoStep 1 (AdvanceAgenda (isSide B attrs -> True)) -> do
      n <- scenarioFieldMap ScenarioTokens (Token.countTokens Token.Horror)
      if n >= 4
        then do
          eachInvestigator \iid -> do
            push $ SufferTrauma iid 0 1
            push $ InvestigatorDefeated (toSource attrs) iid
          push R2
        else do
          replenish <- perPlayer 1
          locations <- select Anywhere
          for_ locations \lid -> do
            clues <- field LocationClues lid
            when (clues < replenish) $ placeTokens attrs lid #clue (replenish - clues)
          spectral <-
            select
              $ LocationWithTrait Spectral
              <> not_ (LocationWithEnemy $ EnemyWithTitle "Jean Devereux")
          lead <- getLead
          for_ spectral \lid -> flipOverBy lead attrs lid
          push $ RevertAgenda attrs.id
      pure a
    _ -> GatheringMists <$> liftRunMessage msg attrs
