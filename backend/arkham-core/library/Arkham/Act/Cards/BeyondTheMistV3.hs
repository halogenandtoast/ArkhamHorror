module Arkham.Act.Cards.BeyondTheMistV3 (
  BeyondTheMistV3 (..),
  beyondTheMistV3,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.CampaignLogKey
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Brazier
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Movement
import Arkham.Scenarios.UnionAndDisillusion.Helpers

newtype BeyondTheMistV3 = BeyondTheMistV3 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

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
  runMessage msg a@(BeyondTheMistV3 attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ skillTestModifier (toSource attrs) SkillTestTarget (Difficulty (-2))
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      push $ AdvanceAct (toId a) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      geistTrap <- getJustLocationByName "The Geist-Trap"
      investigatorsAtUnvisitedIsles <- selectList $ InvestigatorAt (LocationWithTitle "Unvisited Isle")
      players <- getPlayerCount
      lodgeNeophytes <-
        take (if players > 2 then 2 else 1) <$> getSetAsideCardsMatching (cardIs Enemies.lodgeNeophyte)
      creationMessages <-
        traverse (\lodgeNeophyte -> createEnemyAt_ lodgeNeophyte geistTrap Nothing) lodgeNeophytes
      josefIsAliveAndWell <- getHasRecord JosefIsAliveAndWell

      josefMessages <-
        if josefIsAliveAndWell
          then do
            josef <- getSetAsideCard Enemies.josefMeiger
            josefCreation <- createEnemyAt_ josef geistTrap Nothing
            investigators <- getInvestigators
            pure
              $ josefCreation
              : [ gameModifier
                  attrs
                  iid
                  $ CannotParleyWith
                  $ enemyIs Enemies.josefMeiger
                | iid <- investigators
                ]
          else pure []

      pushAll
        $ RevealLocation Nothing geistTrap
        : [Move $ move attrs iid geistTrap | iid <- investigatorsAtUnvisitedIsles]
          <> creationMessages
          <> josefMessages
          <> [NextAdvanceActStep (toId attrs) 1, advanceActDeck attrs]
      pure a
    NextAdvanceActStep aid 1 | aid == toId attrs -> do
      rest <- getSetAsideCardsMatching (CardFromEncounterSet SilverTwilightLodge)
      pushAll [ShuffleCardsIntoDeck Deck.EncounterDeck rest, ShuffleEncounterDiscardBackIn]
      pure a
    _ -> BeyondTheMistV3 <$> runMessage msg attrs
