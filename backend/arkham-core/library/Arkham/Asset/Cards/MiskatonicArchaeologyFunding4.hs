module Arkham.Asset.Cards.MiskatonicArchaeologyFunding4 (
  miskatonicArchaeologyFunding4,
  miskatonicArchaeologyFunding4Effect,
  MiskatonicArchaeologyFunding4 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Trait (Trait (Miskatonic))

newtype MiskatonicArchaeologyFunding4 = MiskatonicArchaeologyFunding4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miskatonicArchaeologyFunding4 :: AssetCard MiskatonicArchaeologyFunding4
miskatonicArchaeologyFunding4 =
  asset MiskatonicArchaeologyFunding4 Cards.miskatonicArchaeologyFunding4

instance HasAbilities MiskatonicArchaeologyFunding4 where
  getAbilities (MiskatonicArchaeologyFunding4 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ForcedAbility
        $ DealtDamageOrHorror
          Timing.When
          AnySource
          You
    ]

slot :: AssetAttrs -> Slot
slot attrs = TraitRestrictedSlot (toSource attrs) Miskatonic []

instance RunMessage MiskatonicArchaeologyFunding4 where
  runMessage msg a@(MiskatonicArchaeologyFunding4 attrs) = case msg of
    CardEnteredPlay iid card | toCardId card == toCardId attrs -> do
      pushAll $ replicate 2 (AddSlot iid AllySlot (slot attrs))
      MiskatonicArchaeologyFunding4 <$> runMessage msg attrs
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push
        $ createCardEffect
          Cards.miskatonicArchaeologyFunding4
          Nothing
          (toSource attrs)
          (InvestigatorTarget iid)
      pure a
    _ -> MiskatonicArchaeologyFunding4 <$> runMessage msg attrs

newtype MiskatonicArchaeologyFunding4Effect = MiskatonicArchaeologyFunding4Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miskatonicArchaeologyFunding4Effect
  :: EffectArgs -> MiskatonicArchaeologyFunding4Effect
miskatonicArchaeologyFunding4Effect =
  cardEffect
    MiskatonicArchaeologyFunding4Effect
    Cards.miskatonicArchaeologyFunding4

instance HasModifiersFor MiskatonicArchaeologyFunding4Effect where
  getModifiersFor target (MiskatonicArchaeologyFunding4Effect a) | effectTarget a == target =
    do
      pure $ toModifiers a [NoMoreThanOneDamageOrHorrorAmongst $ AssetWithTrait Miskatonic]
  getModifiersFor _ _ = pure []

instance RunMessage MiskatonicArchaeologyFunding4Effect where
  runMessage msg e@(MiskatonicArchaeologyFunding4Effect attrs@EffectAttrs {..}) =
    case msg of
      -- TODO: update to after damage
      EndUpkeep -> do
        push (DisableEffect effectId)
        pure e
      _ -> MiskatonicArchaeologyFunding4Effect <$> runMessage msg attrs
