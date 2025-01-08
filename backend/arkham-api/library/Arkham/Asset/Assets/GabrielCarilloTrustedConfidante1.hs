module Arkham.Asset.Assets.GabrielCarilloTrustedConfidante1 (
  gabrielCarilloTrustedConfidante1,
  GabrielCarilloTrustedConfidante1 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Matcher

newtype GabrielCarilloTrustedConfidante1 = GabrielCarilloTrustedConfidante1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gabrielCarilloTrustedConfidante1 :: AssetCard GabrielCarilloTrustedConfidante1
gabrielCarilloTrustedConfidante1 = ally GabrielCarilloTrustedConfidante1 Cards.gabrielCarilloTrustedConfidante1 (2, 1)

instance HasModifiersFor GabrielCarilloTrustedConfidante1 where
  getModifiersFor (GabrielCarilloTrustedConfidante1 a) = controllerGets a [SkillModifier #intellect 1]

instance HasAbilities GabrielCarilloTrustedConfidante1 where
  getAbilities (GabrielCarilloTrustedConfidante1 x) =
    [ controlledAbility x 1 (can.draw.cards You)
        $ ReactionAbility (TurnBegins #when You) (AddCurseTokenCost 1)
    ]

instance RunMessage GabrielCarilloTrustedConfidante1 where
  runMessage msg a@(GabrielCarilloTrustedConfidante1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCardsIfCan iid (attrs.ability 1) 1
      pure a
    _ -> GabrielCarilloTrustedConfidante1 <$> liftRunMessage msg attrs
