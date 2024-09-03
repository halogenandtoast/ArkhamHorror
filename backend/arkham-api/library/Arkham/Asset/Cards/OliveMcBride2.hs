module Arkham.Asset.Cards.OliveMcBride2 (oliveMcBride2, OliveMcBride2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.ChaosBagStepState
import Arkham.Matcher
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype OliveMcBride2 = OliveMcBride2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oliveMcBride2 :: AssetCard OliveMcBride2
oliveMcBride2 = ally OliveMcBride2 Cards.oliveMcBride2 (1, 3)

instance HasAbilities OliveMcBride2 where
  getAbilities (OliveMcBride2 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (WouldRevealChaosToken #when You) (exhaust a)
    ]

toDrawSource :: [Window] -> Source
toDrawSource = \case
  ((windowType -> (Window.WouldRevealChaosToken source _)) : _) -> source
  (_ : rest) -> toDrawSource rest
  _ -> error "Expected WouldRevealChaosToken window"

instance RunMessage OliveMcBride2 where
  runMessage msg a@(OliveMcBride2 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (toDrawSource -> drawSource) _ -> do
      push
        $ ReplaceCurrentDraw drawSource iid
        $ Choose
          (toSource attrs)
          2
          ResolveChoice
          [Undecided Draw, Undecided Draw, Undecided Draw, Undecided Draw]
          []
      cancelledOrIgnoredCardOrGameEffect (attrs.ability 1)
      pure a
    _ -> OliveMcBride2 <$> liftRunMessage msg attrs
