module Arkham.Asset.Cards.Haste2 (
  haste2,
  Haste2 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action (Action)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Window (Window (..), defaultWindows)
import Arkham.Window qualified as Window

newtype Haste2 = Haste2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

haste2 :: AssetCard Haste2
haste2 = asset Haste2 Cards.haste2

-- PerformedSameTypeOfAction
instance HasAbilities Haste2 where
  getAbilities (Haste2 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          ( PerformedSameTypeOfAction
              #when
              You
              AnyAction
          )
          (exhaust a)
    ]

getActionTypes :: [Window] -> [Action]
getActionTypes [] = []
getActionTypes ((windowType -> Window.PerformedSameTypeOfAction _ as) : ws) =
  as <> getActionTypes ws
getActionTypes (_ : ws) = getActionTypes ws

instance RunMessage Haste2 where
  runMessage msg a@(Haste2 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getActionTypes -> as) _ -> do
      actions <- concatMapM (getActions iid) (defaultWindows iid)
      let _available = filter (any (`elem` as) . abilityActions) actions
      pure a
    _ -> Haste2 <$> runMessage msg attrs
