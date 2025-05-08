module Arkham.Location.Cards.TenAcreMeadow_246 (tenAcreMeadow_246) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Runner (locationEnemiesWithTrait)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait

newtype TenAcreMeadow_246 = TenAcreMeadow_246 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tenAcreMeadow_246 :: LocationCard TenAcreMeadow_246
tenAcreMeadow_246 = location TenAcreMeadow_246 Cards.tenAcreMeadow_246 3 (Static 1)

instance HasAbilities TenAcreMeadow_246 where
  getAbilities (TenAcreMeadow_246 a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 (Here <> exists (EnemyAt YourLocation <> withTrait Abomination)) (FastAbility Free)

instance RunMessage TenAcreMeadow_246 where
  runMessage msg l@(TenAcreMeadow_246 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      abominations <- locationEnemiesWithTrait attrs Abomination
      chooseTargetM iid abominations \eid -> do
        placeClues (attrs.ability 1) eid 1
        atEndOfRound (attrs.ability 1) $ removeClues (attrs.ability 1) eid 1
      pure l
    _ -> TenAcreMeadow_246 <$> liftRunMessage msg attrs
