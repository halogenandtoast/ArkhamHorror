module Arkham.Asset.Assets.DaisysToteBagAdvanced (
  daisysToteBagAdvanced,
  daisysToteBagAdvancedEffect,
  DaisysToteBagAdvanced (..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Effect.Import
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets, modified_)
import Arkham.Helpers.Window (cardPlayed)
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Matcher qualified as Matcher
import Arkham.Slot
import Arkham.Trait

newtype DaisysToteBagAdvanced = DaisysToteBagAdvanced AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

daisysToteBagAdvanced :: AssetCard DaisysToteBagAdvanced
daisysToteBagAdvanced = asset DaisysToteBagAdvanced Cards.daisysToteBagAdvanced

instance HasAbilities DaisysToteBagAdvanced where
  getAbilities (DaisysToteBagAdvanced a) =
    [ controlled a 1 (DuringTurn You)
        $ ReactionAbility (Matcher.PlayCard #when You (basic #tome))
        $ (exhaust a)
    ]

instance HasModifiersFor DaisysToteBagAdvanced where
  getModifiersFor (DaisysToteBagAdvanced a) = controllerGets a [CanBecomeFast $ #asset <> #tome]

slot :: AssetAttrs -> Slot
slot attrs = TraitRestrictedSlot (toSource attrs) Tome []

instance RunMessage DaisysToteBagAdvanced where
  runMessage msg a@(DaisysToteBagAdvanced attrs) = runQueueT $ case msg of
    -- Slots need to be added before the asset is played so we hook into played card
    CardIsEnteringPlay iid card | toCardId card == toCardId attrs -> do
      pushAll $ replicate 2 (AddSlot iid HandSlot (slot attrs))
      DaisysToteBagAdvanced <$> liftRunMessage msg attrs
    UseCardAbility _ (isSource attrs -> True) 1 (cardPlayed -> card) _ -> do
      createCardEffect Cards.daisysToteBagAdvanced Nothing (attrs.ability 1) card
      pure a
    _ -> DaisysToteBagAdvanced <$> liftRunMessage msg attrs

newtype DaisysToteBagAdvancedEffect = DaisysToteBagAdvancedEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

daisysToteBagAdvancedEffect :: EffectArgs -> DaisysToteBagAdvancedEffect
daisysToteBagAdvancedEffect = cardEffect DaisysToteBagAdvancedEffect Cards.daisysToteBagAdvanced

instance HasModifiersFor DaisysToteBagAdvancedEffect where
  getModifiersFor (DaisysToteBagAdvancedEffect a) =
    modified_ a a.target [BecomesFast FastPlayerWindow]

instance RunMessage DaisysToteBagAdvancedEffect where
  runMessage msg e@(DaisysToteBagAdvancedEffect attrs) = runQueueT $ case msg of
    CardEnteredPlay _ card | CardIdTarget card.id == attrs.target -> do
      disableReturn e
    _ -> DaisysToteBagAdvancedEffect <$> liftRunMessage msg attrs
