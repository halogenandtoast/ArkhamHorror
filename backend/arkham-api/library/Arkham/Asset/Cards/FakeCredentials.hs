module Arkham.Asset.Cards.FakeCredentials (fakeCredentials, FakeCredentials (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Attack
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Token

newtype Meta = Meta {chosenEnemy :: Maybe EnemyId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype FakeCredentials = FakeCredentials (With AssetAttrs Meta)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fakeCredentials :: AssetCard FakeCredentials
fakeCredentials = asset (FakeCredentials . (`with` Meta Nothing)) Cards.fakeCredentials

instance HasAbilities FakeCredentials where
  getAbilities (FakeCredentials (With x _)) =
    [ skillTestAbility
        $ restrictedAbility x 1 ControlsThis
        $ parleyAction (exhaust x <> ChooseEnemyCost (EnemyAt YourLocation <> CanParleyEnemy You))
    ]

instance RunMessage FakeCredentials where
  runMessage msg a@(FakeCredentials (With attrs meta)) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (chosenEnemyPayment -> eid) -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) iid #intellect
        $ SumCalculation [Fixed 1, AssetTokenCountCalculation attrs.id Suspicion]
      pure $ FakeCredentials $ With attrs (meta {chosenEnemy = eid})
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      discoverAtYourLocation NotInvestigate iid (attrs.ability 1) 1
      placeTokens (attrs.ability 1) attrs Suspicion 1
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      chooseOneM iid do
        labeled "Discard Fake Credentials" $ toDiscardBy iid (attrs.ability 1) attrs
        for_ (chosenEnemy meta) $ \eid -> do
          whenM (lift $ eid <=~> EnemyCanAttack (InvestigatorWithId iid)) do
            labeled "The chosen enemy attacks you" do
              push $ EnemyAttack $ enemyAttack eid (attrs.ability 1) iid
      pure a
    _ -> FakeCredentials . (`with` meta) <$> liftRunMessage msg attrs
