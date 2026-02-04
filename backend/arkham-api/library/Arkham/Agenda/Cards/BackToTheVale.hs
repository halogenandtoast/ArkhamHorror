module Arkham.Agenda.Cards.BackToTheVale (backToTheVale) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Deck qualified as Deck
import Arkham.ForMovement
import Arkham.Helpers.Location (getCanMoveTo, withLocationOf)
import Arkham.Helpers.Scenario (scenarioFieldMap)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Placement
import Arkham.Scenario.Types (Field (..))
import Arkham.Token
import Arkham.Trait (Trait (Dark))

newtype BackToTheVale = BackToTheVale AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backToTheVale :: AgendaCard BackToTheVale
backToTheVale = agenda (3, A) BackToTheVale Cards.backToTheVale (Static 3)

instance HasAbilities BackToTheVale where
  getAbilities (BackToTheVale a) =
    [ mkAbility a 1
        $ FastAbility
          (OrCost [ExhaustAssetCost (AssetWithTitle "Vale Lantern"), GroupClueCost (PerPlayer 1) Anywhere])
    , restricted a 2 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow
    ]

instance RunMessage BackToTheVale where
  runMessage msg a@(BackToTheVale attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      placeTokens attrs ScenarioTarget DarknessLevel 1
      doStep 1 msg
      pure a
    DoStep 1 msg'@(AdvanceAgenda (isSide B attrs -> True)) -> do
      darknessLevel <- scenarioFieldMap ScenarioTokens (countTokens DarknessLevel)
      if darknessLevel >= 6
        then do
          eachInvestigator \iid -> do
            sufferPhysicalTrauma iid 1
            investigatorDefeated attrs iid
          push R2
        else do
          eachInvestigator (`forInvestigator` msg')
          revertAgenda attrs
      pure a
    ForInvestigator iid (AdvanceAgenda (isSide B attrs -> True)) -> do
      enemies <- select $ IncludeOutOfPlayEnemy $ EnemyWithPlacement $ OutOfPlay PursuitZone
      if null enemies
        then discardUntilFirst iid (attrs.ability 1) Deck.EncounterDeck #enemy
        else do
          withLocationOf iid \loc -> do
            locations <- select $ NearestLocationToLocation loc $ LocationWithTrait Dark
            chooseTargetM iid enemies \enemy -> do
              isDark <- matches loc (LocationWithTrait Dark)
              if isDark
                then spawnEnemyAt_ enemy loc
                else chooseOrRunOneM iid do
                  targets locations $ spawnEnemyAt_ enemy
      pure a
    RequestedEncounterCard (isSource attrs -> True) (Just iid) (Just ec) -> do
      withLocationOf iid \loc -> do
        isDark <- matches loc (LocationWithTrait Dark)
        if isDark
          then spawnEnemyAt_ ec loc
          else do
            locations <- select $ NearestLocationToLocation loc $ LocationWithTrait Dark
            chooseOrRunOneM iid $ targets locations $ spawnEnemyAt_ ec
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <-
        select $ ConnectedFrom NotForMovement (locationWithInvestigator iid) <> UnrevealedLocation
      chooseTargetM iid locations \loc -> do
        lookAtRevealed iid (attrs.ability 1) loc
        whenM (getCanMoveTo iid (attrs.ability 1) loc) do
          chooseOneM iid $ withI18n do
            labeled' "move" $ moveTo (attrs.ability 1) iid loc
            skip_
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push R1
      pure a
    _ -> BackToTheVale <$> liftRunMessage msg attrs
