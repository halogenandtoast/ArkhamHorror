module Arkham.Asset.Assets.StringAlong (stringAlong) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Keyword
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype StringAlong = StringAlong AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stringAlong :: AssetCard StringAlong
stringAlong = asset StringAlong Cards.stringAlong

instance HasAbilities StringAlong where
  getAbilities (StringAlong a) = [restricted a 1 ControlsThis $ triggered (AttemptToEvade #when You AnyEnemy) (exhaust a)]

getDetails :: [Window] -> (SkillTestId, EnemyId)
getDetails [] = error "No enemy found"
getDetails ((windowType -> Window.AttemptToEvadeEnemy sid _ eid) : _) = (sid, eid)
getDetails (_ : rest) = getDetails rest

instance RunMessage StringAlong where
  runMessage msg a@(StringAlong attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getDetails -> (sid, enemy)) _ -> do
      skillTestModifier sid (attrs.ability 1) iid (AnySkillValue 1)
      skillTestModifier sid (attrs.ability 1) enemy (AddKeyword Alert)
      onSucceedByEffect sid (atLeast 0) (attrs.ability 1) sid do
        skillTestModifier sid (attrs.ability 1) iid DoNotDisengageEvaded
        drawCards iid (attrs.ability 1) 1
      pure a
    _ -> StringAlong <$> liftRunMessage msg attrs
