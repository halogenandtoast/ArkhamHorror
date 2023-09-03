module Arkham.Act.Cards.BeyondTheMistV1 (
  BeyondTheMistV1 (..),
  beyondTheMistV1,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Location.Brazier
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenarios.UnionAndDisillusion.Helpers

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
      push $ skillTestModifier (toSource attrs) SkillTestTarget (Difficulty (-2))
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      push $ AdvanceAct (toId a) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      pure a
    _ -> BeyondTheMistV1 <$> runMessage msg attrs
