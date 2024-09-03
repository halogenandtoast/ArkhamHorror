module Arkham.Asset.Cards.FleshWard (fleshWard, FleshWard (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Window qualified as Window

newtype FleshWard = FleshWard AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fleshWard :: AssetCard FleshWard
fleshWard = assetWith FleshWard Cards.fleshWard ((healthL ?~ 1) . (sanityL ?~ 1))

instance HasAbilities FleshWard where
  getAbilities (FleshWard a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          (DealtDamageOrHorror #when (SourceIsCancelable $ SourceIsEnemyAttack AnyEnemy) You)
          (exhaust a <> assetUseCost a Charge 1)
    ]

instance RunMessage FleshWard where
  runMessage msg a@(FleshWard attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 windows' _ -> do
      ignoreWindow <- checkAfter $ Window.CancelledOrIgnoredCardOrGameEffect $ attrs.ability 1
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ [ Label "Cancel 1 damage" [CancelDamage iid 1, ignoreWindow]
          | dealtDamage windows' > 0
          ]
        <> [ Label "Cancel 1 horror" [CancelHorror iid 1, ignoreWindow]
           | dealtHorror windows' > 0
           ]
      pure a
    _ -> FleshWard <$> runMessage msg attrs
