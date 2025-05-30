module Arkham.Location.Cards.Parlor where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.TheGathering.Helpers

newtype Parlor = Parlor LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

parlor :: LocationCard Parlor
parlor = location Parlor Cards.parlor 2 (Static 0)

instance HasModifiersFor Parlor where
  getModifiersFor (Parlor a) = whenUnrevealed a $ modifySelf a [Blocked]

instance HasAbilities Parlor where
  getAbilities (Parlor a) =
    extendRevealed
      a
      [ scenarioI18n $ withI18nTooltip "parlor.resign" $ locationResignAction a
      , skillTestAbility
          $ restricted (proxied (assetIs Cards.litaChantler) a) 1 (Uncontrolled <> OnSameLocation) #parley
      ]

instance RunMessage Parlor where
  runMessage msg l@(Parlor attrs) = runQueueT $ case msg of
    UseThisAbility iid source@(isProxySource attrs -> True) 1 -> do
      aid <- selectJust $ assetIs Cards.litaChantler
      sid <- getRandom
      parley sid iid source aid #intellect (Fixed 4)
      pure l
    PassedThisSkillTest iid (isProxySource attrs -> True) -> do
      aid <- selectJust $ assetIs Cards.litaChantler
      takeControlOfAsset iid aid
      pure l
    _ -> Parlor <$> liftRunMessage msg attrs
