module Arkham.Act.Cards.InTheSearchlight (inTheSearchlight) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Cost (getSpendableClueCount)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Modifier
import Arkham.Scenario.Types (Field (..))
import Arkham.Scenarios.SanguineShadows.Helpers
import Arkham.Token qualified as Token

newtype InTheSearchlight = InTheSearchlight ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inTheSearchlight :: ActCard InTheSearchlight
inTheSearchlight = act (2, A) InTheSearchlight Cards.inTheSearchlight Nothing

instance HasAbilities InTheSearchlight where
  getAbilities = actAbilities \a ->
    [ skillTestAbility
        $ restricted
          (proxied (EnemyMatcherSource $ enemyIs Enemies.laChicaRojaTheGirlInTheCarmineCoat) a)
          1
          OnSameLocation
          parleyAction_
    , mkAbility a 2
        $ Objective
        $ forced
        $ EnemyWouldBeDefeated #when (enemyIs Enemies.laChicaRojaTheGirlInTheCarmineCoat)
    ]

instance RunMessage InTheSearchlight where
  runMessage msg a@(InTheSearchlight attrs) = runQueueT $ case msg of
    UseThisAbility iid source@(isProxySource attrs -> True) 1 -> do
      sid <- getRandom
      n <- getSpendableClueCount [iid]
      when (n > 0) $ chooseOneM iid $ withI18n $ countVar 1 do
        labeled' "spendClues" do
          spendClues iid 1
          skillTestModifier sid source sid (Difficulty (-2))
        labeled' "doNotSpendClues" nothing
      chooseBeginSkillTest sid iid (AbilitySource source 1) iid [#intellect, #agility] (Fixed 6)
      pure a
    PassedThisSkillTest iid (isProxyAbilitySource attrs 1 -> True) -> do
      chooseOneM iid do
        abilityLabeled iid (noLimit $ mkAbility attrs 2 $ forced AnyWindow) nothing
      pure a
    FailedThisSkillTest iid (isProxyAbilitySource attrs 1 -> True) -> do
      laChicaRoja <- selectJust $ enemyIs Enemies.laChicaRojaTheGirlInTheCarmineCoat
      initiateEnemyAttack laChicaRoja (attrs.ability 2) iid
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      laChicaRoja <- selectJust $ enemyIs Enemies.laChicaRojaTheGirlInTheCarmineCoat
      withLocationOf laChicaRoja \loc -> do
        targetCount <- scenarioFieldMap ScenarioTokens (Token.countTokens Token.Target)
        hasTarget <- matches loc (LocationWithToken Token.Target)
        if hasTarget
          then do
            removeTokens attrs loc Token.Target 1
            placeTokens attrs ScenarioTarget Token.Target 1
          else do
            locations <- select $ NearestLocationToLocation loc (LocationWithToken Token.Target)
            leadChooseOneM do
              targets locations \targetLocation -> do
                removeTokens attrs targetLocation Token.Target 1
                placeTokens attrs ScenarioTarget Token.Target 1
        if targetCount + 1 >= 3
          then scenarioI18n $ scope "interlude" do
            flavor $ setTitle "title" >> p "castALight1"
          else do
            cancelEnemyDefeatWithWindows laChicaRoja
            healAllDamage attrs laChicaRoja
            place laChicaRoja InTheShadows
            lead <- getLead
            resolveConcealed lead laChicaRoja
            push $ ResetActDeckToStage 1
            push $ ResetAgendaDeckToStage 1
            eachInvestigator (`loseAllClues` attrs)
            selectEach Anywhere (placeCluesUpToClueValue attrs)
      pure a
    _ -> InTheSearchlight <$> liftRunMessage msg attrs
