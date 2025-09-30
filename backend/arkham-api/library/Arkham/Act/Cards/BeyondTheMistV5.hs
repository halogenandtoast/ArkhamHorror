module Arkham.Act.Cards.BeyondTheMistV5 (beyondTheMistV5) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.GameValue
import Arkham.Helpers.Query (getJustLocationByName)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Location.Brazier
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Modifier (ModifierType (..))
import Arkham.Scenarios.UnionAndDisillusion.Helpers

newtype BeyondTheMistV5 = BeyondTheMistV5 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beyondTheMistV5 :: ActCard BeyondTheMistV5
beyondTheMistV5 = act (3, A) BeyondTheMistV5 Cards.beyondTheMistV5 Nothing

instance HasAbilities BeyondTheMistV5 where
  getAbilities = actAbilities \x ->
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

instance RunMessage BeyondTheMistV5 where
  runMessage msg a@(BeyondTheMistV5 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid attrs sid (Difficulty (-2))
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      geistTrap <- getJustLocationByName "The Geist-Trap"
      investigatorsAtUnvisitedIsles <- select $ InvestigatorAt (LocationWithTitle "Unvisited Isle")
      reveal geistTrap
      for_ investigatorsAtUnvisitedIsles \iid -> moveTo attrs iid geistTrap

      anetteMason <- getSetAsideCard Enemies.anetteMason
      n <- perPlayer 2
      createEnemyWithAfter_ anetteMason geistTrap \anetteId -> do
        placeTokens attrs anetteId #damage n

      advanceActDeck attrs
      pure a
    _ -> BeyondTheMistV5 <$> liftRunMessage msg attrs
