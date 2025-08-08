module Arkham.Asset.Assets.ForbiddenTomeSecretsRevealed3 (forbiddenTomeSecretsRevealed3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype ForbiddenTomeSecretsRevealed3 = ForbiddenTomeSecretsRevealed3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forbiddenTomeSecretsRevealed3 :: AssetCard ForbiddenTomeSecretsRevealed3
forbiddenTomeSecretsRevealed3 = asset ForbiddenTomeSecretsRevealed3 Cards.forbiddenTomeSecretsRevealed3

instance HasModifiersFor ForbiddenTomeSecretsRevealed3 where
  getModifiersFor (ForbiddenTomeSecretsRevealed3 a) = for_ a.controller \iid -> do
    selectOne (AbilityIs (toSource a) 1) >>= traverse_ \ab -> do
      handCount <- getHandCount iid
      let n = handCount `div` 4
      modifiedWhen_ a (n > 0) (AbilityTarget iid ab.ref) [ActionCostModifier (-n)]

instance HasAbilities ForbiddenTomeSecretsRevealed3 where
  getAbilities (ForbiddenTomeSecretsRevealed3 a) =
    [ controlled
        a
        1
        (exists $ oneOf [AccessibleLocation, YourLocation <> LocationWithAnyClues])
        $ ActionAbility [] (ActionCost 4 <> exhaust a)
    ]

instance RunMessage ForbiddenTomeSecretsRevealed3 where
  runMessage msg a@(ForbiddenTomeSecretsRevealed3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lids <- getAccessibleLocations iid attrs
      chooseOrRunOneM iid do
        labeled "Do not move" nothing
        targets lids $ moveTo (attrs.ability 1) iid
      discoverAtYourLocation NotInvestigate iid (attrs.ability 1) 1
      pure a
    _ -> ForbiddenTomeSecretsRevealed3 <$> liftRunMessage msg attrs
