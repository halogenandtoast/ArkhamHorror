module Arkham.Location.Cards.DestroyedPath (destroyedPath) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.GameValue
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Location.Cards qualified as Cards (destroyedPath)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.WhereDoomAwaits.Helpers

newtype DestroyedPath = DestroyedPath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

destroyedPath :: LocationCard DestroyedPath
destroyedPath = location DestroyedPath Cards.destroyedPath 3 (Static 0)

instance HasAbilities DestroyedPath where
  getAbilities (DestroyedPath a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ RevealLocation #after You (be a)
      , scenarioI18n
          $ withI18nTooltip "destroyedPath.investigate"
          $ investigateAbility a 2 mempty Here
      ]

instance RunMessage DestroyedPath where
  runMessage msg l@(DestroyedPath attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      amount <- perPlayer 1
      placeDoom (attrs.ability 1) attrs amount
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      investigate sid iid (attrs.ability 2)
      pure l
    Successful (Action.Investigate, _) _ (isAbilitySource attrs 2 -> True) _ _ -> do
      removeDoom (attrs.ability 2) attrs 1
      pure l
    _ -> DestroyedPath <$> liftRunMessage msg attrs
