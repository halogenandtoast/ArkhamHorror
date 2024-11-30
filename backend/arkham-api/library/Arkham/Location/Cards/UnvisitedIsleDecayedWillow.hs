module Arkham.Location.Cards.UnvisitedIsleDecayedWillow (
  unvisitedIsleDecayedWillow,
  UnvisitedIsleDecayedWillow (..),
)
where

import Arkham.Action qualified as Action
import Arkham.CampaignLogKey
import Arkham.GameValue
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers
import Arkham.Location.Brazier
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Scenarios.UnionAndDisillusion.Helpers

newtype UnvisitedIsleDecayedWillow = UnvisitedIsleDecayedWillow LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unvisitedIsleDecayedWillow :: LocationCard UnvisitedIsleDecayedWillow
unvisitedIsleDecayedWillow = location UnvisitedIsleDecayedWillow Cards.unvisitedIsleDecayedWillow 4 (PerPlayer 2)

instance HasModifiersFor UnvisitedIsleDecayedWillow where
  getModifiersFor target (UnvisitedIsleDecayedWillow attrs) = maybeModified attrs do
    guard $ attrs `isTarget` target
    guard $ not attrs.revealed
    sidedWithLodge <- lift $ getHasRecord TheInvestigatorsSidedWithTheLodge
    isLit <- lift $ selectAny $ locationIs Locations.forbiddingShore <> LocationWithBrazier Lit
    guard $ if sidedWithLodge then not isLit else isLit
    pure [Blocked]

instance HasAbilities UnvisitedIsleDecayedWillow where
  getAbilities (UnvisitedIsleDecayedWillow attrs) =
    extendRevealed
      attrs
      [ restricted attrs 1 Here $ ActionAbility ([Action.Circle]) $ ActionCost 1
      , haunted "Choose and discard 1 card from your hand." attrs 2
      ]

instance RunMessage UnvisitedIsleDecayedWillow where
  runMessage msg l@(UnvisitedIsleDecayedWillow attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- genId
      circleTest sid iid (attrs.ability 1) attrs [#intellect, #combat] (Fixed 9)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ toMessage $ chooseAndDiscardCard iid attrs
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      passedCircleTest iid attrs
      pure l
    _ -> UnvisitedIsleDecayedWillow <$> runMessage msg attrs
