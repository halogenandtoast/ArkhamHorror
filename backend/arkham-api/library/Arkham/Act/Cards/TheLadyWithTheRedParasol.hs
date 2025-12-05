module Arkham.Act.Cards.TheLadyWithTheRedParasol (theLadyWithTheRedParasol) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Agenda.Sequence qualified as AS
import Arkham.Agenda.Sequence qualified as Agenda
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Campaigns.TheScarletKeys.Concealed.Kind
import Arkham.Campaigns.TheScarletKeys.Key
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Agenda (currentAgendaSequenceIs)
import Arkham.Helpers.Cost (getCanAffordCost, payEffectCost)
import Arkham.Helpers.Enemy (cancelEnemyEngagement)
import Arkham.Helpers.FlavorText
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Move
import Arkham.Message.Lifted.Placement
import Arkham.Scenarios.ShadesOfSuffering.Helpers
import Arkham.Trait (Trait (Clairvoyant, Geist, Sorcerer))

newtype TheLadyWithTheRedParasol = TheLadyWithTheRedParasol ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theLadyWithTheRedParasol :: ActCard TheLadyWithTheRedParasol
theLadyWithTheRedParasol = act (1, A) TheLadyWithTheRedParasol Cards.theLadyWithTheRedParasol Nothing

instance HasAbilities TheLadyWithTheRedParasol where
  getAbilities = actAbilities1 \a ->
    mkAbility a 1
      $ Objective
      $ forced
      $ EnemyEngaged #when You (enemyIs Enemies.tzuSanNiangTheLadyWithTheRedParasol)

instance RunMessage TheLadyWithTheRedParasol where
  runMessage msg a@(TheLadyWithTheRedParasol attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let cost = GroupClueCost (PerPlayer 1) Anywhere
      canAfford <- getCanAffordCost iid (attrs.ability 1) [] [] cost
      if canAfford
        then do
          payEffectCost iid attrs cost
          afterMaybeSkillTest $ advancedWithClues attrs
        else doStep 1 msg
      pure a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      selectEach ConcealedCardAny removeFromGame
      tzuSanNiang <- selectJust $ enemyIs Enemies.tzuSanNiangTheLadyWithTheRedParasol
      cancelEnemyEngagement iid tzuSanNiang
      place tzuSanNiang InTheShadows
      lead <- getLead
      resolveConcealed lead tzuSanNiang
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      melatisShop <- placeSetAsideLocation Locations.melatisShop
      reveal melatisShop
      selectEach AnyEnemy disengageFromAll
      moveAllTo attrs melatisShop
      sorcererOrClairvoyant <- selectAny $ mapOneOf InvestigatorWithTrait [Sorcerer, Clairvoyant]
      flintRejoinedTheCell <- getHasRecord FlintRejoinedTheCell
      scenarioI18n $ scope "theBomoh" do
        flavor do
          setTitle "title"
          p "theBomoh1Part1"
          p.validate sorcererOrClairvoyant "sorcererOrClairvoyant"
          p "theBomoh1Part2"
          ul do
            li.validate flintRejoinedTheCell "flintRejoinedTheCell"
            li.validate (not flintRejoinedTheCell) "agentFlintIsMissing"

        if flintRejoinedTheCell
          then flavor $ setTitle "title" >> p "theBomoh2"
          else do
            crossOut AgentFlintIsMissing
            record AgentFlintIsDead
            theShadeReaper <- selectJust $ scarletKeyIs Keys.theShadeReaper
            placeTokens attrs theShadeReaper #charge 2
            flavor $ setTitle "title" >> p "theBomoh3"

        flavor $ setTitle "title" >> p "theBomoh4"
        tinMine <- placeSetAsideLocation Locations.tinMine
        wayangKulitTheater <- placeSetAsideLocation Locations.wayangKulitTheater
        slainForeman <-
          fetchCard
            [SetAsideCard Enemies.slainForemanSympathyPain, SetAsideCard Enemies.slainForemanFamilialPain]
        uncannyShadow <-
          fetchCard
            [ SetAsideCard Enemies.uncannyShadowPlayfulShadows
            , SetAsideCard Enemies.uncannyShadowTimorousShadows
            ]
        buriedMiner <-
          fetchCard
            [SetAsideCard Enemies.buriedMinerALostMemento, SetAsideCard Enemies.buriedMinerExhumeTheBones]

        kualaLumpurStationWestWing <- selectJust $ locationIs Locations.kualaLumpurStationWestWing
        createEnemyAt_ slainForeman kualaLumpurStationWestWing
        createEnemyAt_ uncannyShadow wayangKulitTheater
        createEnemyAt_ buriedMiner tinMine

      selectEach ConcealedCardAny removeFromGame
      doStep 1 msg
      whenM (currentAgendaSequenceIs (== AS.Sequence 1 AS.A)) do
        do_ $ AdvanceToAgenda 1 Agendas.restlessDead Agenda.A (toSource attrs)
      advanceActDeck attrs
      pure a
    DoStep 1 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      tzuSanNiang <- selectJust $ enemyIs Enemies.tzuSanNiangTheLadyWithTheRedParasol
      place tzuSanNiang InTheShadows
      locations <- select $ LocationWithEnemy $ EnemyWithTrait Geist <> NonWeaknessEnemy
      gatherConcealedCards tzuSanNiang >>= \case
        Just (TzuSanNiang, cards) -> do
          for_ cards $ push . CreateConcealedCard
          distributeEvenlyBetween cards locations
        _ -> error "failed to gather tzu san niang's concealed cards"
      push
        $ RemoveAllDoomFromPlay
          defaultRemoveDoomMatchers {removeDoomEnemies = InPlayEnemy (not_ $ EnemyWithId tzuSanNiang)}
      pure a
    _ -> TheLadyWithTheRedParasol <$> liftRunMessage msg attrs
