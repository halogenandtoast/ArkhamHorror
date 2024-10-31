module Arkham.Location.Cards.TenAcreMeadow_246 (
  tenAcreMeadow_246,
  tenAcreMeadow_246Effect,
  TenAcreMeadow_246 (..),
) where

import Arkham.Ability
import Arkham.Effect.Import
import Arkham.Exception
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (tenAcreMeadow_246)
import Arkham.Location.Import.Lifted
import Arkham.Location.Runner (locationEnemiesWithTrait, pattern RemoveClues)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait

newtype TenAcreMeadow_246 = TenAcreMeadow_246 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tenAcreMeadow_246 :: LocationCard TenAcreMeadow_246
tenAcreMeadow_246 =
  location TenAcreMeadow_246 Cards.tenAcreMeadow_246 3 (Static 1)

instance HasAbilities TenAcreMeadow_246 where
  getAbilities (TenAcreMeadow_246 attrs) =
    extendRevealed1 attrs
      $ groupLimit PerGame
      $ restricted
        attrs
        1
        (Here <> exists (EnemyAt YourLocation <> withTrait Abomination))
        (FastAbility Free)

instance RunMessage TenAcreMeadow_246 where
  runMessage msg l@(TenAcreMeadow_246 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      abominations <- locationEnemiesWithTrait attrs Abomination
      when
        (null abominations)
        (throwIO $ InvalidState "should not have been able to use this ability")
      chooseTargetM iid abominations \eid -> do
        placeClues (attrs.ability 1) eid 1
        createCardEffect Cards.tenAcreMeadow_246 Nothing (attrs.ability 1) eid
      pure l
    _ -> TenAcreMeadow_246 <$> liftRunMessage msg attrs

newtype TenAcreMeadow_246Effect = TenAcreMeadow_246Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tenAcreMeadow_246Effect :: EffectArgs -> TenAcreMeadow_246Effect
tenAcreMeadow_246Effect = cardEffect TenAcreMeadow_246Effect Cards.tenAcreMeadow_246

instance RunMessage TenAcreMeadow_246Effect where
  runMessage msg e@(TenAcreMeadow_246Effect attrs) = runQueueT $ case msg of
    EndRound -> do
      push $ RemoveClues attrs.source attrs.target 1
      disableReturn e
    _ -> TenAcreMeadow_246Effect <$> liftRunMessage msg attrs
