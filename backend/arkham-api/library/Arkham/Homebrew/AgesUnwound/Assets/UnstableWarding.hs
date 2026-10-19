module Arkham.Homebrew.AgesUnwound.Assets.UnstableWarding (unstableWarding) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.Classes.HasQueue (HasQueue, removeAllMessagesMatching)
import Arkham.GameValue
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.AgesUnwound.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Stories qualified as Stories
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Window qualified as Window
import Control.Monad.Trans (MonadTrans)

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
that. The pending defeat is cancelled and the story read instead.
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

{- | Drop the defeat the engine queued when the last horror landed.

The frame @Asset.Runner@ pushes for a defeat -- the when window, @When@, @Do@,
@After@, the after window -- is not a batch, so 'cancelWindowBatch' over the
defeat window finds no batch id and silently does nothing, and the @Do@
discards the card out from under the story that is about to remove it from the
game. /Backfire/ then has nothing in play to remove and the warding is left in
the encounter discard.

So pop the frame outright, the way /Machinations Through Time/ cancels a
Scientist's defeat to abduct them instead (@Scenarios.MachinationsThroughTime.
Helpers.abductById@, which carries the same note).
-}
cancelPendingDefeat :: (MonadTrans t, HasQueue Msg.Message m) => AssetId -> t m ()
cancelPendingDefeat aid = lift $ removeAllMessagesMatching \case
  Msg.When (Msg.AssetDefeated _ aid') -> aid == aid'
  Msg.Do (Msg.AssetDefeated _ aid') -> aid == aid'
  Msg.After (Msg.AssetDefeated _ aid') -> aid == aid'
  Msg.CheckWindows ws -> any isDefeatWindow ws
  _ -> False
 where
  isDefeatWindow w = case Window.windowType w of
    Window.AssetDefeated aid' _ -> aid == aid'
    _ -> False

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
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      cancelPendingDefeat attrs.id
      flipper <- maybe getLead pure (lastTester attrs)
      readStory flipper attrs Stories.backfire
      pure a
    _ -> UnstableWarding <$> liftRunMessage msg attrs
