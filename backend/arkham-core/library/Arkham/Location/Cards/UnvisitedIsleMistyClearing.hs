module Arkham.Location.Cards.UnvisitedIsleMistyClearing (
  unvisitedIsleMistyClearing,
  UnvisitedIsleMistyClearing (..),
)
where

import Arkham.Prelude

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
import Arkham.Scenarios.UnionAndDisillusion.Helpers

newtype UnvisitedIsleMistyClearing = UnvisitedIsleMistyClearing LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unvisitedIsleMistyClearing :: LocationCard UnvisitedIsleMistyClearing
unvisitedIsleMistyClearing = location UnvisitedIsleMistyClearing Cards.unvisitedIsleMistyClearing 1 (PerPlayer 2)

instance HasModifiersFor UnvisitedIsleMistyClearing where
  getModifiersFor target (UnvisitedIsleMistyClearing attrs)
    | attrs `isTarget` target
    , not (locationRevealed attrs) = do
        sidedWithLodge <- getHasRecord TheInvestigatorsSidedWithTheLodge
        isLit <- selectAny $ locationIs Locations.forbiddingShore <> LocationWithBrazier Lit
        pure
          [ toModifier attrs Blocked
          | if sidedWithLodge then not isLit else isLit
          ]
  getModifiersFor _ _ = pure []

instance HasAbilities UnvisitedIsleMistyClearing where
  getAbilities (UnvisitedIsleMistyClearing attrs) =
    withRevealedAbilities
      attrs
      [ restrictedAbility attrs 1 Here $ ActionAbility ([Action.Circle]) $ ActionCost 1
      , haunted "You must either place 1 doom on this location, or take 1 damage and 1 horror" attrs 2
      ]

instance RunMessage UnvisitedIsleMistyClearing where
  runMessage msg l@(UnvisitedIsleMistyClearing attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      sid <- getRandom
      circleTest sid iid attrs attrs [#willpower, #agility] (Fixed 11)
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ Label "Place 1 doom on this location" [PlaceDoom (toSource attrs) (toTarget attrs) 1]
          , Label "Take 1 damage and 1 horror" [InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 1]
          ]
      pure l
    PassedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ -> do
      passedCircleTest iid attrs
      pure l
    _ -> UnvisitedIsleMistyClearing <$> runMessage msg attrs
