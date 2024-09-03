module Arkham.Asset.Cards.PeterClover (
  peterClover,
  PeterClover (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Trait

newtype PeterClover = PeterClover AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

peterClover :: AssetCard PeterClover
peterClover =
  allyWith PeterClover Cards.peterClover (3, 2)
    $ (slotsL .~ [])
    . (isStoryL .~ True)

instance HasAbilities PeterClover where
  getAbilities (PeterClover x) =
    [ restrictedAbility x 1 Uncontrolled $ ForcedAbility $ PhaseBegins #when #enemy
    , restrictedAbility
        x
        2
        (ControlsThis <> enemyExists (EnemyAt YourLocation <> EnemyWithTrait Criminal))
        (FastAbility $ exhaust x)
    ]

instance RunMessage PeterClover where
  runMessage msg a@(PeterClover attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ DealAssetDamage (toId attrs) (toAbilitySource attrs 1) 1 0
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      criminals <- select $ EnemyWithTrait Criminal <> EnemyAt YourLocation
      player <- getPlayer iid
      push $ chooseOne player $ targetLabels criminals (only . EnemyEvaded iid)
      pure a
    _ -> PeterClover <$> runMessage msg attrs
