module Arkham.Act.Cards.BeyondTheMistV2 (beyondTheMistV2) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.ChaosToken
import Arkham.Helpers.Query
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Location.Brazier
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message.Lifted.Move
import Arkham.Modifier
import Arkham.Scenarios.UnionAndDisillusion.Helpers

newtype BeyondTheMistV2 = BeyondTheMistV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beyondTheMistV2 :: ActCard BeyondTheMistV2
beyondTheMistV2 = act (3, A) BeyondTheMistV2 Cards.beyondTheMistV2 Nothing

instance HasAbilities BeyondTheMistV2 where
  getAbilities = actAbilities \x ->
    [ restricted x 1 DuringCircleAction $ FastAbility $ ClueCost (Static 1)
    , restricted
        x
        2
        ( AllLocationsMatch
            (LocationWithUnrevealedTitle "Unvisited Isle")
            (RevealedLocation <> LocationWithBrazier Unlit)
        )
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage BeyondTheMistV2 where
  runMessage msg a@(BeyondTheMistV2 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) sid (Difficulty (-2))
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      geistTrap <- getJustLocationByName "The Geist-Trap"
      reveal geistTrap
      selectEach (InvestigatorAt "Unvisited Isle") \iid -> moveTo attrs iid geistTrap
      removeAllChaosTokens Cultist
      advanceActDeck attrs
      pure a
    _ -> BeyondTheMistV2 <$> liftRunMessage msg attrs
