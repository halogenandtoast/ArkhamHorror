module Arkham.Asset.Assets.AzizaSteerHardboiled (azizaSteerHardboiled) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Event.Types (Field (..))
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Trait (Trait (Insight, Tactic, Trick))
import Arkham.Window (Window)
import Arkham.Window qualified as Window

newtype AzizaSteerHardboiled = AzizaSteerHardboiled AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

azizaSteerHardboiled :: AssetCard AzizaSteerHardboiled
azizaSteerHardboiled = ally AzizaSteerHardboiled Cards.azizaSteerHardboiled (3, 3)

instance HasAbilities AzizaSteerHardboiled where
  getAbilities (AzizaSteerHardboiled a) =
    [ controlled a 1 (DuringTurn You)
        $ freeReaction
        $ PlayEvent #after You (EventOneOf $ map EventWithTrait [Insight, Tactic, Trick])
    ]

-- | The event this ability triggered on.
playedEvent :: [Window] -> Maybe EventId
playedEvent ws = listToMaybe [eid | (Window.windowType -> Window.PlayEvent _ eid) <- ws]

{- | "After you play an event during your turn, exhaust Aziza Steer. If that event
was..." -- one event can carry more than one of the three traits, so each clause
is checked rather than branched on.
-}
instance RunMessage AzizaSteerHardboiled where
  runMessage msg a@(AzizaSteerHardboiled attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (playedEvent -> Just event) _ -> do
      exhaustThis attrs
      traits <- field EventTraits event
      when (Insight `member` traits) $ discoverAtYourLocation NotInvestigate iid (attrs.ability 1) 1
      when (Tactic `member` traits) do
        enemies <- select $ enemyAtLocationWith iid
        chooseTargetM iid enemies \enemy ->
          nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1 enemy
      when (Trick `member` traits) $ gainResources iid (attrs.ability 1) 2
      pure a
    _ -> AzizaSteerHardboiled <$> liftRunMessage msg attrs
