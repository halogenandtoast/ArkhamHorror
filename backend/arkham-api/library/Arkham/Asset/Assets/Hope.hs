module Arkham.Asset.Assets.Hope (hope) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (AssetCard)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Projection

newtype Hope = Hope AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hope :: AssetCard Hope
hope = asset Hope Cards.hope

instance HasAbilities Hope where
  getAbilities (Hope a) =
    [ controlled a 1 (exists $ oneOf [assetIs Cards.zeal, assetIs Cards.augur])
        $ forced
        $ AssetEntersPlay #when (be a)
    , controlled a 2 (exists $ be a <> AssetReady) $ evadeAction $ OrCost [exhaust a, discardCost a]
    ]

instance RunMessage Hope where
  runMessage msg a@(Hope attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectEach (mapOneOf assetIs [Cards.zeal, Cards.augur]) (toDiscardBy iid (attrs.ability 1))
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = attrs.ability 2
      discarded <- selectNone $ AssetWithId attrs.id
      sid <- getRandom
      skillTestModifier sid source iid (BaseSkillOf #agility 5)
      when discarded $ skillTestModifier sid source sid SkillTestAutomaticallySucceeds
      chooseEvadeEnemy sid iid source
      when discarded do
        hopeCard <- field AssetCard attrs.id
        catsInDiscard <-
          fieldMap InvestigatorDiscard (filterCards (mapOneOf cardIs [Cards.zeal, Cards.augur])) iid
        chooseOrRunOneM iid do
          questionLabeled "Put into play from discard"
          for_ catsInDiscard $ \card -> do
            cardLabeled card do
              shuffleCardsIntoDeck iid (only hopeCard)
              putCardIntoPlay iid card
          labeled "Skip" nothing
      pure a
    _ -> Hope <$> liftRunMessage msg attrs
