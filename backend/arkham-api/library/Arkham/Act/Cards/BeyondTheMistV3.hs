module Arkham.Act.Cards.BeyondTheMistV3 (BeyondTheMistV3 (..), beyondTheMistV3) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.CampaignLogKey
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getJustLocationByName, getPlayerCount, getSetAsideCardsMatching)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Location.Brazier
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message.Lifted.Move
import Arkham.Modifier
import Arkham.Scenarios.UnionAndDisillusion.Helpers

newtype BeyondTheMistV3 = BeyondTheMistV3 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beyondTheMistV3 :: ActCard BeyondTheMistV3
beyondTheMistV3 = act (3, A) BeyondTheMistV3 Cards.beyondTheMistV3 Nothing

instance HasAbilities BeyondTheMistV3 where
  getAbilities (BeyondTheMistV3 x)
    | onSide A x =
        [ restrictedAbility x 1 DuringCircleAction $ FastAbility $ ClueCost (Static 1)
        , restrictedAbility
            x
            2
            ( AllLocationsMatch
                (LocationWithUnrevealedTitle "Unvisited Isle")
                (RevealedLocation <> LocationWithBrazier Unlit)
            )
            $ Objective
            $ ForcedAbility AnyWindow
        ]
  getAbilities _ = []

instance RunMessage BeyondTheMistV3 where
  runMessage msg a@(BeyondTheMistV3 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) sid (Difficulty (-2))
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      geistTrap <- getJustLocationByName "The Geist-Trap"
      reveal geistTrap
      investigatorsAtUnvisitedIsles <- select $ InvestigatorAt (LocationWithTitle "Unvisited Isle")
      for_ investigatorsAtUnvisitedIsles \iid -> moveTo attrs iid geistTrap
      players <- getPlayerCount
      lodgeNeophytes <-
        take (if players > 2 then 2 else 1) <$> getSetAsideCardsMatching (cardIs Enemies.lodgeNeophyte)
      for_ lodgeNeophytes (`createEnemyAt_` geistTrap)

      whenHasRecord JosefIsAliveAndWell do
        josef <- getSetAsideCard Enemies.josefMeiger
        createEnemyAt_ josef geistTrap
        eachInvestigator \iid -> gameModifier attrs iid $ CannotParleyWith $ enemyIs Enemies.josefMeiger

      push $ NextAdvanceActStep attrs.id 1
      advanceActDeck attrs
      pure a
    NextAdvanceActStep aid 1 | aid == attrs.id -> do
      shuffleCardsIntoDeck Deck.EncounterDeck
        =<< getSetAsideCardsMatching (CardFromEncounterSet SilverTwilightLodge)
      shuffleEncounterDiscardBackIn
      pure a
    _ -> BeyondTheMistV3 <$> liftRunMessage msg attrs
