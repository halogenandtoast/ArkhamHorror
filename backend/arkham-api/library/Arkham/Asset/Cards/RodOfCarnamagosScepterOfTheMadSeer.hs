module Arkham.Asset.Cards.RodOfCarnamagosScepterOfTheMadSeer (
  rodOfCarnamagosScepterOfTheMadSeer,
  RodOfCarnamagosScepterOfTheMadSeer (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.ChaosBag.RevealStrategy
import Arkham.Helpers.Investigator (searchBondedFor)
import Arkham.Matcher
import Arkham.Placement
import Arkham.RequestedChaosTokenStrategy
import Arkham.Trait (Trait (Rot))

newtype Meta = Meta {chosenEnemy :: Maybe EnemyId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype RodOfCarnamagosScepterOfTheMadSeer = RodOfCarnamagosScepterOfTheMadSeer (AssetAttrs `With` Meta)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rodOfCarnamagosScepterOfTheMadSeer :: AssetCard RodOfCarnamagosScepterOfTheMadSeer
rodOfCarnamagosScepterOfTheMadSeer =
  asset
    (RodOfCarnamagosScepterOfTheMadSeer . (`with` Meta Nothing))
    Cards.rodOfCarnamagosScepterOfTheMadSeer

instance HasAbilities RodOfCarnamagosScepterOfTheMadSeer where
  getAbilities (RodOfCarnamagosScepterOfTheMadSeer (With attrs _)) =
    [ restrictedAbility attrs 1 ControlsThis
        $ FastAbility (ChooseEnemyCost (NonEliteEnemy <> EnemyAt Anywhere) <> exhaust attrs)
    ]

instance RunMessage RodOfCarnamagosScepterOfTheMadSeer where
  runMessage msg a@(RodOfCarnamagosScepterOfTheMadSeer (With attrs meta)) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (chosenEnemyPayment -> eid) -> do
      push $ RequestChaosTokens (toSource attrs) (Just iid) (Reveal 5) SetAside
      pure . RodOfCarnamagosScepterOfTheMadSeer $ With attrs (meta {chosenEnemy = eid})
    RequestedChaosTokens source (Just iid) tokens | isSource attrs source -> do
      for_ (chosenEnemy meta) \eid -> do
        when (any ((== #curse) . (.face)) tokens) do
          rots <- searchBondedFor iid (CardWithTrait Rot)
          case nonEmpty rots of
            Just rots' -> do
              rot <- sample rots'
              push $ CreateEventAt iid rot (AttachedToEnemy eid)
            _ -> pure ()
      push $ ResetChaosTokens source
      pure a
    _ -> RodOfCarnamagosScepterOfTheMadSeer . (`with` meta) <$> liftRunMessage msg attrs
