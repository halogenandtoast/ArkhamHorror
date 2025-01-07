module Arkham.Act.Cards.BeyondTheMistV1 ( beyondTheMistV1,) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheCircleUndone.Key
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Brazier
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Modifier (ModifierType(..))
import Arkham.Message.Lifted.Move
import Arkham.Helpers.Query (getSetAsideCardsMatching, getPlayerCount, getJustLocationByName)
import Arkham.Helpers.SkillTest (withSkillTest)
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
        [ restricted x 1 DuringCircleAction $ FastAbility $ ClueCost (Static 1)
        , restricted
            x
            2
            ( AllLocationsMatch
                (LocationWithUnrevealedTitle "Unvisited Isle")
                (RevealedLocation <> LocationWithBrazier Lit)
            )
            $ Objective
            $ forced AnyWindow
        ]
  getAbilities _ = []

instance RunMessage BeyondTheMistV1 where
  runMessage msg a@(BeyondTheMistV1 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        skillTestModifier sid (toSource attrs) (SkillTestTarget sid) (Difficulty (-2))
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      geistTrap <- getJustLocationByName "The Geist-Trap"
      investigatorsAtUnvisitedIsles <- select $ InvestigatorAt (LocationWithTitle "Unvisited Isle")
      anetteMason <- getSetAsideCard Enemies.anetteMason
      players <- getPlayerCount
      covenInitiates <-
        take (if players > 2 then 2 else 1) <$> getSetAsideCardsMatching (cardIs Enemies.covenInitiate)

      reveal geistTrap
      for_ investigatorsAtUnvisitedIsles \iid -> moveTo attrs iid geistTrap

      witchesSpellWasCast <- getHasRecord TheWitches'SpellWasCast
      if witchesSpellWasCast
        then createEnemyAt_ anetteMason geistTrap
        else for_ covenInitiates (`createEnemyAt_` geistTrap)

      doStep 1 msg
      advanceActDeck attrs
      pure a
    DoStep 1 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      witches <- getSetAsideCardsMatching (CardWithTrait Witch)
      shuffleCardsIntoDeck Deck.EncounterDeck witches
      shuffleEncounterDiscardBackIn
      pure a
    _ -> BeyondTheMistV1 <$> liftRunMessage msg attrs
