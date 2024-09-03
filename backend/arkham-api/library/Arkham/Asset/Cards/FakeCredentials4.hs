module Arkham.Asset.Cards.FakeCredentials4 (fakeCredentials4, FakeCredentials4 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Matcher
import Arkham.Token

newtype Meta = Meta {chosenEnemy :: Maybe EnemyId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype FakeCredentials4 = FakeCredentials4 (With AssetAttrs Meta)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fakeCredentials4 :: AssetCard FakeCredentials4
fakeCredentials4 = asset (FakeCredentials4 . (`with` Meta Nothing)) Cards.fakeCredentials4

instance HasAbilities FakeCredentials4 where
  getAbilities (FakeCredentials4 (With x _)) =
    [ skillTestAbility
        $ controlledAbility x 1 (exists $ EnemyAt YourLocation)
        $ parleyAction (ChooseEnemyCost $ EnemyAt YourLocation)
    ]

instance RunMessage FakeCredentials4 where
  runMessage msg a@(FakeCredentials4 (With attrs meta)) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (chosenEnemyPayment -> eid) -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) iid #intellect
        $ AssetTokenCountCalculation attrs.id Suspicion
      pure $ FakeCredentials4 $ With attrs (meta {chosenEnemy = eid})
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      discoverAtMatchingLocation
        NotInvestigate
        iid
        (attrs.ability 1)
        (oneOf [locationWithInvestigator iid, ConnectedTo (locationWithInvestigator iid)])
        1
      when (n < 2) do
        placeTokens (attrs.ability 1) attrs Suspicion 1
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      returnToHand iid attrs
      pure a
    _ -> FakeCredentials4 . (`with` meta) <$> liftRunMessage msg attrs
