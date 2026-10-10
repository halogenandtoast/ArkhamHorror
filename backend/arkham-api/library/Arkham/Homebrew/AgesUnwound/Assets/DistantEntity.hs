module Arkham.Homebrew.AgesUnwound.Assets.DistantEntity (distantEntity) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.AgesUnwound.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Stories qualified as Stories
import Arkham.Matcher
import Arkham.Placement
import Arkham.Token (countTokens)
import Arkham.Token qualified as Token

newtype DistantEntity = DistantEntity AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | /Entreating the Gods/ attaches this to Sydney.
distantEntity :: AssetCard DistantEntity
distantEntity = asset DistantEntity Cards.distantEntity

hostLocation :: AssetAttrs -> LocationMatcher
hostLocation a = case a.placement of
  AttachedToLocation lid -> LocationWithId lid
  AtLocation lid -> LocationWithId lid
  _ -> Nowhere

{- | "Attached location gains:
\"[action]: Test [willpower] (3). If you succeed, place a resource on Distant
Entity. /
[action][action] If there are 3 resources on Distant Entity: Flip it over and
resolve its text.\""

"Attached location gains" is expressed as abilities on the asset restricted to
investigators at the host location -- the same way Unstable Warding carries the
abilities Scenario III's Rear Corridors "gains".
-}
instance HasAbilities DistantEntity where
  getAbilities (DistantEntity a) =
    [ skillTestAbility $ restricted a 1 (atHost a) actionAbility
    , restricted a 2 (atHost a <> if resources a >= 3 then NoRestriction else Never) doubleActionAbility
    ]

atHost :: AssetAttrs -> Criterion
atHost a = youExist $ InvestigatorAt (hostLocation a)

resources :: AssetAttrs -> Int
resources a = countTokens Token.Resource a.tokens

instance RunMessage DistantEntity where
  runMessage msg a@(DistantEntity attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 3)
      pure a
    PassedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      placeTokens (attrs.ability 1) attrs #resource 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flipOverBy iid (attrs.ability 2) attrs
      pure a
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid attrs Stories.aBlessingFromOnHigh
      pure a
    _ -> DistantEntity <$> liftRunMessage msg attrs
