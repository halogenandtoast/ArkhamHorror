module Arkham.Homebrew.AgesUnwound.Assets.UnstableWarding (unstableWarding) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.GameValue
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.AgesUnwound.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Stories qualified as Stories
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement

newtype UnstableWarding = UnstableWarding AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | The reverse of /Backfire/ (@:ages-unwound:231@). Scenario III attaches it to
Rear Corridors; its printed sanity is 2, which the def cannot carry.
-}
unstableWarding :: AssetCard UnstableWarding
unstableWarding = assetWith UnstableWarding Cards.unstableWarding (sanityL ?~ 2)

-- | The investigator who put the last horror on it, i.e. who flips it.
lastTester :: AssetAttrs -> Maybe InvestigatorId
lastTester a = toResultDefault Nothing a.meta

{- | "[action] Investigators at this location spend 1[per_investigator] clues as a
group: Test [willpower] or [combat] (1[per_investigator]). If you succeed, add 1
resource to Unstable Warding. If you fail, place 1 horror on Unstable Warding.
Then, if Unstable Warding has no remaining sanity, flip it and resolve its
text."

Ability 2 is the engine hook for that last sentence, so it is silent: an asset
with no remaining sanity is defeated and discarded, and the flip has to replace
that. The defeat batch is cancelled and the story read instead.
-}
instance HasAbilities UnstableWarding where
  getAbilities (UnstableWarding a) =
    [ restricted a 1 (hostLocation a)
        $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) $ hostLocationMatcher a)
    , mkAbility a 2 $ SilentForcedAbility $ AssetDefeated #when ByAny (be a)
    ]
   where
    -- you must be where the warding is to take an action on it
    hostLocation attrs = youExist $ InvestigatorAt (hostLocationMatcher attrs)

hostLocationMatcher :: AssetAttrs -> LocationMatcher
hostLocationMatcher a = case a.placement of
  AttachedToLocation lid -> LocationWithId lid
  AtLocation lid -> LocationWithId lid
  _ -> Nowhere

instance RunMessage UnstableWarding where
  runMessage msg a@(UnstableWarding attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest
        sid
        iid
        (attrs.ability 1)
        attrs
        [#willpower, #combat]
        (GameValueCalculation $ PerPlayer 1)
      pure a
    PassedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      placeTokens (attrs.ability 1) attrs #resource 1
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      placeTokens (attrs.ability 1) attrs #horror 1
      pure $ setMeta (Just iid) a
    UseCardAbility _ (isSource attrs -> True) 2 ws _ -> do
      cancelWindowBatch ws
      flipper <- maybe getLead pure (lastTester attrs)
      readStory flipper attrs Stories.backfire
      pure a
    _ -> UnstableWarding <$> liftRunMessage msg attrs
