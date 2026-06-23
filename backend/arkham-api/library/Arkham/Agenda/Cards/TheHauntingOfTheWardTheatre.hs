module Arkham.Agenda.Cards.TheHauntingOfTheWardTheatre (theHauntingOfTheWardTheatre) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.ClassSymbol
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck
import Arkham.Scenarios.EnthrallingEncore.Helpers

newtype TheHauntingOfTheWardTheatre = TheHauntingOfTheWardTheatre AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHauntingOfTheWardTheatre :: AgendaCard TheHauntingOfTheWardTheatre
theHauntingOfTheWardTheatre =
  agenda (1, A) TheHauntingOfTheWardTheatre Cards.theHauntingOfTheWardTheatre (Static 4)

instance HasAbilities TheHauntingOfTheWardTheatre where
  getAbilities (TheHauntingOfTheWardTheatre a) =
    [ restricted a 1 (exists $ enemyIs Enemies.sinisterSoloist <> EnemyAt YourLocation)
        $ FastAbility'
          (Costs [HandDiscardCost 1 (basic $ CardWithClass c) | c <- [Guardian, Seeker, Rogue, Mystic, Survivor]])
          #parley
    ]

instance RunMessage TheHauntingOfTheWardTheatre where
  runMessage msg a@(TheHauntingOfTheWardTheatre attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      soloist <- selectJust $ enemyIs Enemies.sinisterSoloist
      automaticallyEvadeEnemy iid soloist
      roundModifier (attrs.ability 1) soloist Blank
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      placeTokens attrs ScenarioTarget #resource 1
      doStep 1 msg
      pure a
    DoStep 1 msg'@(AdvanceAgenda (isSide B attrs -> True)) -> do
      measures <- getMeasures
      if measures >= 5
        then do
          eachInvestigator \iid -> do
            sufferMentalTrauma iid 1
            defeat attrs iid
          pure a
        else do
          investigators <- select UneliminatedInvestigator
          for_ investigators \iid -> do
            sid <- getRandom
            beginSkillTest sid iid attrs iid #willpower (Fixed 3)
          doStep 2 msg'
          pure $ TheHauntingOfTheWardTheatre $ setMeta @(Int, Int) (length investigators, 0) attrs
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignHorror iid attrs 1
      assets <- select $ assetControlledBy iid <> not_ PermanentAsset <> NonWeaknessAsset
      when (notNull assets) do
        chooseTargetM iid assets \asset -> do
          push $ AddToScenarioDeck PropsDeck (toTarget asset)
          shuffleDeck PropsDeck
      let (tested, failed) = getMetaDefault @(Int, Int) (0, 0) attrs
      pure $ TheHauntingOfTheWardTheatre $ setMeta @(Int, Int) (tested, failed + 1) attrs
    DoStep 2 (AdvanceAgenda (isSide B attrs -> True)) -> do
      let (tested, failed) = getMetaDefault @(Int, Int) (0, 0) attrs
      when (tested > 0 && failed >= tested) do
        selectOne (enemyIs Enemies.sinisterSoloist) >>= traverse_ \soloist ->
          healDamage soloist attrs =<< perPlayer 1
      shuffleDeck PropsDeck
      selectEach Anywhere (placeCluesUpToClueValue attrs)
      shuffleEncounterDiscardBackIn
      push $ ResetAgendaDeckToStage 1
      pure a
    ResetAgendaDeckToStage 1 -> do
      attrs' <- liftRunMessage (RevertAgenda attrs.id) attrs
      pure $ TheHauntingOfTheWardTheatre $ attrs' & doomL .~ 0
    _ -> TheHauntingOfTheWardTheatre <$> liftRunMessage msg attrs
