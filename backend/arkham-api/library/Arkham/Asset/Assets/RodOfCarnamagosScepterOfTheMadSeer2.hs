module Arkham.Asset.Assets.RodOfCarnamagosScepterOfTheMadSeer2 (rodOfCarnamagosScepterOfTheMadSeer2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Investigator (searchBondedFor)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Trait (Trait (Rot))

newtype Meta = Meta {chosenEnemy :: Maybe EnemyId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype RodOfCarnamagosScepterOfTheMadSeer2 = RodOfCarnamagosScepterOfTheMadSeer2 (AssetAttrs `With` Meta)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rodOfCarnamagosScepterOfTheMadSeer2 :: AssetCard RodOfCarnamagosScepterOfTheMadSeer2
rodOfCarnamagosScepterOfTheMadSeer2 =
  asset
    (RodOfCarnamagosScepterOfTheMadSeer2 . (`with` Meta Nothing))
    Cards.rodOfCarnamagosScepterOfTheMadSeer2

instance HasAbilities RodOfCarnamagosScepterOfTheMadSeer2 where
  getAbilities (RodOfCarnamagosScepterOfTheMadSeer2 (With attrs _)) =
    [ restricted attrs 1 ControlsThis
        $ FastAbility (ChooseEnemyCost (NonEliteEnemy <> EnemyAt Anywhere) <> exhaust attrs)
    ]

instance RunMessage RodOfCarnamagosScepterOfTheMadSeer2 where
  runMessage msg a@(RodOfCarnamagosScepterOfTheMadSeer2 (With attrs meta)) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (chosenEnemyPayment -> eid) -> do
      requestChaosTokens iid attrs 5
      pure . RodOfCarnamagosScepterOfTheMadSeer2 $ With attrs (meta {chosenEnemy = eid})
    RequestedChaosTokens source (Just iid) tokens | isSource attrs source -> do
      for_ (chosenEnemy meta) \eid -> do
        let curses = count ((== #curse) . (.face)) tokens
        when (curses > 0) do
          rots <- searchBondedFor iid (CardWithTrait Rot)
          focusCards rots do
            chooseUpToNM iid curses "Done attaching Rots" do
              targets rots \rot -> do
                obtainCard rot
                push $ CreateEventAt iid rot (AttachedToEnemy eid)
      pure a
    _ -> RodOfCarnamagosScepterOfTheMadSeer2 . (`with` meta) <$> liftRunMessage msg attrs
