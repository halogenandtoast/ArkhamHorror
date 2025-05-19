module Arkham.Asset.Assets.RiteOfSeeking (riteOfSeeking) where

import Arkham.Ability
import Arkham.Aspect hiding (aspect)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Investigate
import Arkham.Matcher
import Arkham.Modifier

newtype RiteOfSeeking = RiteOfSeeking AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riteOfSeeking :: AssetCard RiteOfSeeking
riteOfSeeking = asset RiteOfSeeking Cards.riteOfSeeking

instance HasAbilities RiteOfSeeking where
  getAbilities (RiteOfSeeking a) = [investigateAbility a 1 (assetUseCost a Charge 1) ControlsThis]

instance RunMessage RiteOfSeeking where
  runMessage msg a@(RiteOfSeeking attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      let tokens = oneOf [#skull, #cultist, #tablet, #elderthing, #autofail]
      sid <- getRandom

      onRevealChaosTokenEffect sid tokens source attrs do
        afterThisTestResolves sid do
          setActions iid attrs 0
          endYourTurn iid

      skillTestModifier sid (attrs.ability 1) iid (DiscoveredClues 1)
      aspect iid source (#willpower `InsteadOf` #intellect) (mkInvestigate sid iid source)
      pure a
    _ -> RiteOfSeeking <$> liftRunMessage msg attrs
