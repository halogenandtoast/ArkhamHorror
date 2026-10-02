module Arkham.Homebrew.DarkMatter.Assets.NuclearPowerBank (nuclearPowerBank) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Matcher

newtype NuclearPowerBank = NuclearPowerBank AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nuclearPowerBank :: AssetCard NuclearPowerBank
nuclearPowerBank = asset NuclearPowerBank Cards.nuclearPowerBank

{- | "Resources on Nuclear Power Bank may be spent to pay for events played by
any investigator at your location."
-}
instance HasModifiersFor NuclearPowerBank where
  getModifiersFor (NuclearPowerBank a) = for_ a.controller \iid ->
    controllerGets
      a
      [CanSpendUsesAsResourceOnCardFromInvestigator a.id #resource (colocatedWith iid) #event]

{- | "[forced] When the round ends: Add 2 resource tokens on this card. If it has
5 or more resources, discard it and deal 2 damage to each enemy and investigator
at your location."
-}
instance HasAbilities NuclearPowerBank where
  getAbilities (NuclearPowerBank a) = [controlled_ a 1 $ forced $ RoundEnds #when]

instance RunMessage NuclearPowerBank where
  runMessage msg a@(NuclearPowerBank attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      placeTokens (attrs.ability 1) attrs #resource 2
      when (attrs.use #resource + 2 >= 5) do
        investigators <- select $ colocatedWith iid
        for_ investigators \iid' -> assignDamage iid' (attrs.ability 1) 2
        enemies <- select $ enemyAtLocationWith iid
        for_ enemies $ nonAttackEnemyDamage (Just iid) (attrs.ability 1) 2
        toDiscardBy iid (attrs.ability 1) attrs
      pure a
    _ -> NuclearPowerBank <$> liftRunMessage msg attrs
