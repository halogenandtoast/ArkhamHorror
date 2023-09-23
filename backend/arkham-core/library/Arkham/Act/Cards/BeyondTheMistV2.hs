module Arkham.Act.Cards.BeyondTheMistV2 (
  BeyondTheMistV2 (..),
  beyondTheMistV2,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Location.Brazier
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message
import Arkham.Movement
import Arkham.Scenarios.UnionAndDisillusion.Helpers

newtype BeyondTheMistV2 = BeyondTheMistV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beyondTheMistV2 :: ActCard BeyondTheMistV2
beyondTheMistV2 = act (3, A) BeyondTheMistV2 Cards.beyondTheMistV2 Nothing

instance HasAbilities BeyondTheMistV2 where
  getAbilities (BeyondTheMistV2 x)
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

instance RunMessage BeyondTheMistV2 where
  runMessage msg a@(BeyondTheMistV2 attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ skillTestModifier (toSource attrs) SkillTestTarget (Difficulty (-2))
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      push $ AdvanceAct (toId a) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      geistTrap <- getJustLocationByName "The Geist-Trap"
      investigatorsAtUnvisitedIsles <- selectList $ InvestigatorAt (LocationWithTitle "Unvisited Isle")
      pushAll
        $ RevealLocation Nothing geistTrap
        : [Move $ move attrs iid geistTrap | iid <- investigatorsAtUnvisitedIsles]
          <> [RemoveAllChaosTokens Cultist, advanceActDeck attrs]
      pure a
    _ -> BeyondTheMistV2 <$> runMessage msg attrs
