module Arkham.Location.Cards.UnvisitedIsleDecayedWillow (
  unvisitedIsleDecayedWillow,
  UnvisitedIsleDecayedWillow (..),
)
where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.CampaignLogKey
import Arkham.GameValue
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers

-- import Arkham.Helpers.Message.Discard
import Arkham.Location.Brazier
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Scenarios.UnionAndDisillusion.Helpers

newtype UnvisitedIsleDecayedWillow = UnvisitedIsleDecayedWillow LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unvisitedIsleDecayedWillow :: LocationCard UnvisitedIsleDecayedWillow
unvisitedIsleDecayedWillow = location UnvisitedIsleDecayedWillow Cards.unvisitedIsleDecayedWillow 4 (PerPlayer 2)

instance HasModifiersFor UnvisitedIsleDecayedWillow where
  getModifiersFor target (UnvisitedIsleDecayedWillow attrs)
    | attrs `isTarget` target
    , not (locationRevealed attrs) = do
        sidedWithLodge <- getHasRecord TheInvestigatorsSidedWithTheLodge
        isLit <- selectAny $ locationIs Locations.forbiddingShore <> LocationWithBrazier Lit
        pure
          [ toModifier attrs Blocked
          | if sidedWithLodge then not isLit else isLit
          ]
  getModifiersFor _ _ = pure []

instance HasAbilities UnvisitedIsleDecayedWillow where
  getAbilities (UnvisitedIsleDecayedWillow attrs) =
    withRevealedAbilities
      attrs
      [ restrictedAbility attrs 1 Here $ ActionAbility ([Action.Circle]) $ ActionCost 1
      , haunted "Choose and discard 1 card from your hand." attrs 2
      ]

instance RunMessage UnvisitedIsleDecayedWillow where
  runMessage msg l@(UnvisitedIsleDecayedWillow attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      circleTest iid attrs attrs [#intellect, #combat] (Fixed 9)
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push $ toMessage $ chooseAndDiscardCard iid attrs
      pure l
    PassedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ -> do
      passedCircleTest iid attrs
      pure l
    _ -> UnvisitedIsleDecayedWillow <$> runMessage msg attrs
