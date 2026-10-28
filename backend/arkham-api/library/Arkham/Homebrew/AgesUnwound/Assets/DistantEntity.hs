module Arkham.Homebrew.AgesUnwound.Assets.DistantEntity (distantEntity) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.AgesUnwound.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Token (countTokens)
import Arkham.Token qualified as Token

newtype DistantEntity = DistantEntity AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

distantEntity :: AssetCard DistantEntity
distantEntity = asset DistantEntity Cards.distantEntity

hostLocation :: AssetAttrs -> LocationMatcher
hostLocation a = case a.placement of
  AttachedToLocation lid -> LocationWithId lid
  AtLocation lid -> LocationWithId lid
  _ -> Nowhere

instance HasAbilities DistantEntity where
  getAbilities (DistantEntity a) =
    [ campaignI18n
        $ withI18nTooltip "distantEntity.test"
        $ skillTestAbility
        $ restricted (proxied (hostLocation a) a) 1 Here actionAbility
    , campaignI18n
        $ withI18nTooltip "distantEntity.flip"
        $ restricted
          (proxied (hostLocation a) a)
          2
          (Here <> if resources a >= 3 then NoRestriction else Never)
          doubleActionAbility
    ]

resources :: AssetAttrs -> Int
resources a = countTokens Token.Resource a.tokens

instance RunMessage DistantEntity where
  runMessage msg a@(DistantEntity attrs) = runQueueT $ case msg of
    UseThisAbility iid source@(isProxySource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (AbilitySource source 1) attrs #willpower (Fixed 3)
      pure a
    PassedThisSkillTest _iid (isProxyAbilitySource attrs 1 -> True) -> do
      placeTokens (attrs.ability 1) attrs #resource 1
      pure a
    UseThisAbility iid source@(isProxySource attrs -> True) 2 -> do
      flipOverBy iid (AbilitySource source 2) attrs
      pure a
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid attrs Stories.aBlessingFromOnHigh
      pure a
    _ -> DistantEntity <$> liftRunMessage msg attrs
