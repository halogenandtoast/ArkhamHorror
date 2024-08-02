module Arkham.Asset.Cards.OliveMcBride (oliveMcBride, OliveMcBride (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.ChaosBagStepState
import Arkham.Matcher
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype OliveMcBride = OliveMcBride AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oliveMcBride :: AssetCard OliveMcBride
oliveMcBride = ally OliveMcBride Cards.oliveMcBride (1, 3)

instance HasAbilities OliveMcBride where
  getAbilities (OliveMcBride a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (WouldRevealChaosToken #when You) (exhaust a)
    ]

toDrawSource :: [Window] -> Source
toDrawSource = \case
  ((windowType -> (Window.WouldRevealChaosToken source _)) : _) -> source
  (_ : rest) -> toDrawSource rest
  _ -> error "Expected WouldRevealChaosToken window"

instance RunMessage OliveMcBride where
  runMessage msg a@(OliveMcBride attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (toDrawSource -> drawSource) _ -> do
      push
        $ ReplaceCurrentDraw drawSource iid
        $ Choose
          (toSource attrs)
          2
          ResolveChoice
          [Undecided Draw, Undecided Draw, Undecided Draw]
          []
      cancelledOrIgnoredCardOrGameEffect (attrs.ability 1)
      pure a
    _ -> OliveMcBride <$> liftRunMessage msg attrs
