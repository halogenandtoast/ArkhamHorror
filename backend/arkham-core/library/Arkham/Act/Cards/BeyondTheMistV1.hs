module Arkham.Act.Cards.BeyondTheMistV1 (
  BeyondTheMistV1 (..),
  beyondTheMistV1,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.CampaignLogKey
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Brazier
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Movement
import Arkham.Scenarios.UnionAndDisillusion.Helpers
import Arkham.Trait (Trait (Witch))

newtype BeyondTheMistV1 = BeyondTheMistV1 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beyondTheMistV1 :: ActCard BeyondTheMistV1
beyondTheMistV1 = act (3, A) BeyondTheMistV1 Cards.beyondTheMistV1 Nothing

instance HasAbilities BeyondTheMistV1 where
  getAbilities (BeyondTheMistV1 x)
    | onSide A x =
        [ restrictedAbility x 1 DuringCircleAction $ FastAbility $ ClueCost (Static 1)
        , restrictedAbility
            x
            2
            ( AllLocationsMatch
                (LocationWithUnrevealedTitle "Unvisited Isle")
                (RevealedLocation <> LocationWithBrazier Lit)
            )
            $ Objective
            $ ForcedAbility AnyWindow
        ]
  getAbilities _ = []

instance RunMessage BeyondTheMistV1 where
  runMessage msg a@(BeyondTheMistV1 attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      withSkillTest \sid -> do
        push $ skillTestModifier sid (toSource attrs) (SkillTestTarget sid) (Difficulty (-2))
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      push $ AdvanceAct (toId a) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      geistTrap <- getJustLocationByName "The Geist-Trap"
      investigatorsAtUnvisitedIsles <- select $ InvestigatorAt (LocationWithTitle "Unvisited Isle")
      witchesSpellWasCast <- getHasRecord TheWitches'SpellWasCast
      anetteMason <- getSetAsideCard Enemies.anetteMason
      players <- getPlayerCount
      covenInitiates <-
        take (if players > 2 then 2 else 1) <$> getSetAsideCardsMatching (cardIs Enemies.covenInitiate)

      creationMessages <-
        if witchesSpellWasCast
          then (: []) <$> createEnemyAt_ anetteMason geistTrap Nothing
          else
            traverse
              (\covenInitiate -> createEnemyAt_ covenInitiate geistTrap Nothing)
              covenInitiates

      pushAll
        $ RevealLocation Nothing geistTrap
        : [Move $ move attrs iid geistTrap | iid <- investigatorsAtUnvisitedIsles]
          <> creationMessages
          <> [NextAdvanceActStep (toId attrs) 1, advanceActDeck attrs]
      pure a
    NextAdvanceActStep aid 1 | aid == toId attrs -> do
      witches <- getSetAsideCardsMatching (CardWithTrait Witch)
      pushAll [ShuffleCardsIntoDeck Deck.EncounterDeck witches, ShuffleEncounterDiscardBackIn]
      pure a
    _ -> BeyondTheMistV1 <$> runMessage msg attrs
