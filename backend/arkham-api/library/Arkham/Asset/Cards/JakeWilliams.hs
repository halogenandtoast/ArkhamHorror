module Arkham.Asset.Cards.JakeWilliams (
  jakeWilliams,
  JakeWilliams (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (RevealLocation)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Timing qualified as Timing

newtype JakeWilliams = JakeWilliams AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jakeWilliams :: AssetCard JakeWilliams
jakeWilliams = ally JakeWilliams Cards.jakeWilliams (3, 2)

instance HasModifiersFor JakeWilliams where
  getModifiersFor (InvestigatorTarget iid) (JakeWilliams a) | controlledBy a iid = do
    actions <- fieldMap InvestigatorActionsTaken concat iid
    pure $ toModifiers a $ do
      action <- [Action.Move, Action.Investigate]
      guard $ action `notElem` actions
      pure $ ActionDoesNotCauseAttacksOfOpportunity action
  getModifiersFor _ _ = pure []

instance HasAbilities JakeWilliams where
  getAbilities (JakeWilliams a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          ( OrWindowMatcher
              [ RevealLocation Timing.After You Anywhere
              , PutLocationIntoPlay Timing.After You Anywhere
              ]
          )
          (exhaust a)
    ]

instance RunMessage JakeWilliams where
  runMessage msg a@(JakeWilliams attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ drawCards iid (toAbilitySource attrs 1) 1
      pure a
    _ -> JakeWilliams <$> runMessage msg attrs
