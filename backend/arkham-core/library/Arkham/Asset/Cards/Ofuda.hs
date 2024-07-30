module Arkham.Asset.Cards.Ofuda (ofuda, Ofuda (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Enemy.Types (Field (..))
import Arkham.Game.Helpers (onSameLocation)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Modifier
import Arkham.Projection
import Arkham.Token

-- Notes: This implementation assumes in order to lose a keyword, you must have it.

newtype Ofuda = Ofuda AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ofuda :: AssetCard Ofuda
ofuda = asset Ofuda Cards.ofuda

instance HasAbilities Ofuda where
  getAbilities (Ofuda a) =
    [ restrictedAbility a 1 ControlsThis
        $ FastAbility
        $ ChooseEnemyCost
          ( NonEliteEnemy
              <> EnemyAt YourLocation
              <> oneOf (map EnemyWithKeyword [Keyword.Alert, Keyword.Aloof, Keyword.Elusive, Keyword.Retaliate])
          )
        <> assetUseCost a Charge 1
    ]

instance RunMessage Ofuda where
  runMessage msg a@(Ofuda attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 _ (chosenEnemyPayment -> Just eid) -> do
      keywords <- field EnemyKeywords eid
      roundModifiers (attrs.ability 1) eid
        $ map RemoveKeyword
        $ filter (`elem` keywords) [Keyword.Alert, Keyword.Aloof, Keyword.Elusive, Keyword.Retaliate]
      pure a
    BeginRound -> do
      pure . Ofuda $ attrs & setMeta True
    RevealChaosToken _ iid token | token.face == #bless -> do
      sameLocation <- onSameLocation iid attrs.placement
      let canReplenish = getAssetMetaDefault True attrs && attrs.use Charge == 0 && sameLocation
      if canReplenish
        then do
          push $ AddUses (toSource attrs) attrs.id Charge 1
          pure . Ofuda $ attrs & setMeta False
        else pure a
    _ -> Ofuda <$> liftRunMessage msg attrs
