module Arkham.Act.Cards.InTheSearchlight (inTheSearchlight) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Cost (getSpendableClueCount)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location
import Arkham.Helpers.Log
import Arkham.Helpers.Query (getInvestigators)
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Modifier
import Arkham.Projection
import Arkham.ScenarioLogKey
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
        targetCount <- countScenarioTokens Token.Target
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
          then delayIfSkillTest $ scenarioI18n $ scope "interlude" do
            n <- fieldMap EnemyTokens (Token.countTokens Token.Target) laChicaRoja
            flavor do
              setTitle "title"
              p "castALight1"
              ul $ li.nested "checkTargets" do
                li.validate (n == 0) "noTargets"
                li.validate (n /= 0) "otherwise"
            if n == 0
              then doStep 2 msg
              else push R1
          else do
            cancelEnemyDefeatWithWindows laChicaRoja
            healAllDamage attrs laChicaRoja
            place laChicaRoja InTheShadows
            doStep 0 msg -- resolveConcealed lead laChicaRoja needs the target to be removed first
            push $ ResetActDeckToStage 1
            push $ ResetAgendaDeckToStage 1
            eachInvestigator (`loseAllClues` attrs)
            selectEach Anywhere (placeCluesUpToClueValue attrs)
      pure a
    DoStep 0 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      lead <- getLead
      laChicaRoja <- selectJust $ enemyIs Enemies.laChicaRojaTheGirlInTheCarmineCoat
      resolveConcealed lead laChicaRoja
      pure a
    DoStep 2 msg'@(AdvanceAct (isSide B attrs -> True) _ _) -> scenarioI18n $ scope "interlude" do
      storyWithChooseOneM' (setTitle "title" >> p "castALight2") do
        labeled' "believe" $ doStep 3 msg'
        labeled' "doNotBelieve" $ doStep 4 msg'
      pure a
    DoStep 3 msg'@(AdvanceAct (isSide B attrs -> True) _ _) -> scenarioI18n $ scope "interlude" do
      trust <- remembered MatiasBolivarTrustsYou
      distrust <- remembered MatiasBolivarDoesntTrustYou
      flavor do
        setTitle "title"
        p "castALight3"
        ul do
          li.validate trust "trustsYou"
          li.validate distrust "doesNotTrustYou"
          li.validate (not $ trust || distrust) "neither"
      if
        | trust -> doStep 5 msg'
        | distrust -> doStep 6 msg'
        | otherwise -> doStep 7 msg'
      pure a
    DoStep 4 (AdvanceAct (isSide B attrs -> True) _ _) -> scenarioI18n $ scope "interlude" do
      flavor $ setTitle "title" >> p "castALight4"
      push R1
      pure a
    DoStep 5 msg'@(AdvanceAct (isSide B attrs -> True) _ _) -> scenarioI18n $ scope "interlude" do
      flavor $ setTitle "title" >> p "castALight5"
      doStep 7 msg'
      pure a
    DoStep 6 msg'@(AdvanceAct (isSide B attrs -> True) _ _) -> scenarioI18n $ scope "interlude" do
      flavor $ setTitle "title" >> p "castALight6"
      doStep 7 msg'
      pure a
    DoStep 7 (AdvanceAct (isSide B attrs -> True) _ _) -> scenarioI18n $ scope "interlude" do
      flavor $ setTitle "title" >> p "castALight7"
      laChicaRoja <- selectJust $ enemyIs Enemies.laChicaRojaTheGirlInTheCarmineCoat
      removeEnemy laChicaRoja
      investigators <- getInvestigators
      laChicaRojaAsset <- createAssetAt Assets.laChicaRojaYourWatchfulShadow Unplaced
      leadChooseOneM do
        questionLabeledCard Assets.laChicaRojaYourWatchfulShadow
        portraits investigators (`takeControlOfAsset` laChicaRojaAsset)
      agenda <- fetchCard Agendas.seeingRed
      push $ SetCurrentAgendaDeck 1 [agenda]
      placeDoomOnAgenda 1
      locations <- shuffle =<< select (LocationWithToken Token.Target)
      case nonEmpty locations of
        Nothing -> error "No locations"
        Just (x :| rest) -> do
          createEnemyAt_ Enemies.theSanguineWatcherWithTheRubySpectacles x
          for_ rest (removeAllOfTokenOn attrs Token.Target)
      lead <- getLead
      drawCard lead =<< fetchCard Enemies.apportionedKa
      shuffleEncounterDiscardBackIn
      removeAct attrs
      pure a
    _ -> InTheSearchlight <$> liftRunMessage msg attrs
