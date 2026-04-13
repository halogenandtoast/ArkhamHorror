module Arkham.Asset.Assets.RodOfCarnamagosScepterOfTheMadSeer (rodOfCarnamagosScepterOfTheMadSeer) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Investigator (searchBondedFor)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Trait (Trait (Rot))

newtype RodOfCarnamagosScepterOfTheMadSeer = RodOfCarnamagosScepterOfTheMadSeer AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rodOfCarnamagosScepterOfTheMadSeer :: AssetCard RodOfCarnamagosScepterOfTheMadSeer
rodOfCarnamagosScepterOfTheMadSeer =
  asset RodOfCarnamagosScepterOfTheMadSeer Cards.rodOfCarnamagosScepterOfTheMadSeer

instance HasAbilities RodOfCarnamagosScepterOfTheMadSeer where
  getAbilities (RodOfCarnamagosScepterOfTheMadSeer attrs) =
    [ controlled_ attrs 1
        $ freeTrigger (ChooseEnemyCost (NonEliteEnemy <> EnemyAt Anywhere) <> exhaust attrs)
    ]

instance RunMessage RodOfCarnamagosScepterOfTheMadSeer where
  runMessage msg a@(RodOfCarnamagosScepterOfTheMadSeer attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (chosenEnemyPayment -> eid) -> do
      requestChaosTokens iid (attrs.ability 1) 5
      pure . RodOfCarnamagosScepterOfTheMadSeer $ attrs & setMeta eid
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      continue_ iid
      for_ (getAssetMeta attrs) \eid -> do
        when (any ((== #curse) . (.face)) tokens) do
          rots <- searchBondedFor iid (CardWithTrait Rot)
          for_ (nonEmpty rots) \rots' -> do
            rot <- sample rots'
            obtainCard rot
            push $ CreateEventAt iid rot (AttachedToEnemy eid)
      push $ ReturnChaosTokens tokens
      pure a
    _ -> RodOfCarnamagosScepterOfTheMadSeer <$> liftRunMessage msg attrs
