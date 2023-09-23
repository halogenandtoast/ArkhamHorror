module Arkham.Act.Cards.BeyondTheMistV4 (
  BeyondTheMistV4 (..),
  beyondTheMistV4,
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
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Location.Brazier
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message
import Arkham.Movement
import Arkham.Scenarios.UnionAndDisillusion.Helpers

newtype BeyondTheMistV4 = BeyondTheMistV4 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beyondTheMistV4 :: ActCard BeyondTheMistV4
beyondTheMistV4 = act (3, A) BeyondTheMistV4 Cards.beyondTheMistV4 Nothing

instance HasAbilities BeyondTheMistV4 where
  getAbilities (BeyondTheMistV4 x)
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

instance RunMessage BeyondTheMistV4 where
  runMessage msg a@(BeyondTheMistV4 attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ skillTestModifier (toSource attrs) SkillTestTarget (Difficulty (-2))
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      push $ AdvanceAct (toId a) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      geistTrap <- getJustLocationByName "The Geist-Trap"
      investigatorsAtUnvisitedIsles <- selectList $ InvestigatorAt (LocationWithTitle "Unvisited Isle")

      silverTwilightLodge <- getSetAsideCardsMatching (CardFromEncounterSet SilverTwilightLodge)

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
          <> [ShuffleCardsIntoDeck Deck.EncounterDeck silverTwilightLodge, ShuffleEncounterDiscardBackIn]
          <> josefMessages
          <> [NextAdvanceActStep (toId attrs) 1, advanceActDeck attrs]
      pure a
    _ -> BeyondTheMistV4 <$> runMessage msg attrs
