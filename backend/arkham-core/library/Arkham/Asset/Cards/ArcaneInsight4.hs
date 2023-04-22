module Arkham.Asset.Cards.ArcaneInsight4
  ( arcaneInsight4
  , ArcaneInsight4(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Types
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher hiding ( DuringTurn )
import Arkham.Projection

newtype ArcaneInsight4 = ArcaneInsight4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneInsight4 :: AssetCard ArcaneInsight4
arcaneInsight4 = asset ArcaneInsight4 Cards.arcaneInsight4

instance HasAbilities ArcaneInsight4 where
  getAbilities (ArcaneInsight4 a) =
    [ limitedAbility (PlayerLimit PerTurn 1)
        $ restrictedAbility a 1 (ControlsThis <> DuringTurn Anyone)
        $ FastAbility
        $ UseCost (AssetWithId $ toId a) Charge 1
    ]

instance RunMessage ArcaneInsight4 where
  runMessage msg a@(ArcaneInsight4 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      mlid <- field InvestigatorLocation iid
      for_ mlid $ \lid ->
        push $ CreateWindowModifierEffect
          EffectTurnWindow
          (EffectModifiers $ toModifiers attrs [ShroudModifier (-2)])
          (toSource attrs)
          (LocationTarget lid)
      pure a
    _ -> ArcaneInsight4 <$> runMessage msg attrs
