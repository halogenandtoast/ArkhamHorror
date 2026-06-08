module Arkham.Story.Cards.DriveOffTheMiGo (driveOffTheMiGo) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card (flipCard, toCard)
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Query
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Placement
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype DriveOffTheMiGo = DriveOffTheMiGo StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

driveOffTheMiGo :: StoryCard DriveOffTheMiGo
driveOffTheMiGo = story DriveOffTheMiGo Cards.driveOffTheMiGo & persistStory

instance HasAbilities DriveOffTheMiGo where
  getAbilities (DriveOffTheMiGo a) =
    [ mkAbility a 1 $ forced $ EnemyDefeated #when Anyone ByAny (enemyIs Enemies.miGoGeneral)
    , restricted a 2 (exists $ LocationWithTitle "Fungus Mound" <> LocationWithoutClues)
        $ forced AnyWindow
    , restricted
        a
        3
        (exists $ LocationWithTitle "Fungus Mound" <> LocationWithClues (GreaterThanOrEqualTo $ PerPlayer 4))
        $ forced AnyWindow
    ]

instance RunMessage DriveOffTheMiGo where
  runMessage msg s@(DriveOffTheMiGo attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      fungusMound <- getJustLocationByName "Fungus Mound"
      createEnemyAt_ Enemies.miGoGeneral fungusMound
      n <- getPlayerCount
      placeClues (toSource attrs) fungusMound n
      pure $ DriveOffTheMiGo $ attrs & placementL .~ Global
    UseThisAbility iid (isSource attrs -> True) n | n `elem` [1, 2] -> do
      remember TheMiGoWereDrivenOff
      flipOver iid attrs
      pure s
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      remember TheSecretOfTheOozeWasStolen
      flipOver iid attrs
      pure s
    Flip iid _ (isTarget attrs -> True) -> do
      chooseOneM iid $ targeting attrs nothing

      whenM (remembered TheSecretOfTheOozeWasStolen) do
        selectEach (enemyIs Enemies.miGoGeneral) removeFromGame
        -- The bonus is done via the MiGo Drone directly
        push $ PlaceNextTo ScenarioTarget [flipCard $ toCard attrs]
        removeFromGame attrs

      whenM (remembered TheMiGoWereDrivenOff) do
        weapon <- getSetAsideCard Assets.miGoWeapon
        investigators <- allInvestigators
        leadChooseOrRunOneM $ withI18n do
          nameVar Assets.miGoWeapon $ questionLabeled' "takeControlOf"
          questionLabeledCard Assets.miGoWeapon
          portraits investigators (`takeControlOfSetAsideAsset` weapon)
        selectEach (enemyIs Enemies.miGoGeneral) (addToVictory iid)
        addToVictory iid attrs
        push $ RemoveAllCopiesOfEncounterCardFromGame (cardIs Enemies.miGoDrone)
        placeTokens (attrs.ability 1) ScenarioTarget #resource 1

      pure $ DriveOffTheMiGo $ attrs & flippedL .~ True
    _ -> DriveOffTheMiGo <$> liftRunMessage msg attrs
