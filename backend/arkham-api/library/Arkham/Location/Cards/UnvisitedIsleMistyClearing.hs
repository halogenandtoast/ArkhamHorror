module Arkham.Location.Cards.UnvisitedIsleMistyClearing (
  unvisitedIsleMistyClearing,
  UnvisitedIsleMistyClearing (..),
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

newtype UnvisitedIsleMistyClearing = UnvisitedIsleMistyClearing LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unvisitedIsleMistyClearing :: LocationCard UnvisitedIsleMistyClearing
unvisitedIsleMistyClearing = location UnvisitedIsleMistyClearing Cards.unvisitedIsleMistyClearing 1 (PerPlayer 2)

instance HasModifiersFor UnvisitedIsleMistyClearing where
  getModifiersFor target (UnvisitedIsleMistyClearing attrs) = maybeModified attrs do
    guard $ attrs `isTarget` target
    guard $ not attrs.revealed
    sidedWithLodge <- getHasRecord TheInvestigatorsSidedWithTheLodge
    isLit <- selectAny $ locationIs Locations.forbiddingShore <> LocationWithBrazier Lit
    guard $ if sidedWithLodge then not isLit else isLit
    pure [Blocked]

instance HasAbilities UnvisitedIsleMistyClearing where
  getAbilities (UnvisitedIsleMistyClearing attrs) =
    extendRevealed
      attrs
      [ restricted attrs 1 Here $ ActionAbility ([Action.Circle]) $ ActionCost 1
      , haunted "You must either place 1 doom on this location, or take 1 damage and 1 horror" attrs 2
      ]

instance RunMessage UnvisitedIsleMistyClearing where
  runMessage msg l@(UnvisitedIsleMistyClearing attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- genId
      circleTest sid iid (attrs.ability 1) attrs [#willpower, #agility] (Fixed 11)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ Label "Place 1 doom on this location" [PlaceDoom (toSource attrs) (toTarget attrs) 1]
          , Label "Take 1 damage and 1 horror" [InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 1]
          ]
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      passedCircleTest iid attrs
      pure l
    _ -> UnvisitedIsleMistyClearing <$> runMessage msg attrs
