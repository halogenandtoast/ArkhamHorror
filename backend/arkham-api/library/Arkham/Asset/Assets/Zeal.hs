module Arkham.Asset.Assets.Zeal (zeal, Zeal (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Fight
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (AssetCard)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Projection

newtype Zeal = Zeal AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zeal :: AssetCard Zeal
zeal = asset Zeal Cards.zeal

instance HasAbilities Zeal where
  getAbilities (Zeal a) =
    [ controlledAbility a 1 (exists $ oneOf [assetIs Cards.hope, assetIs Cards.augur])
        $ forced
        $ AssetEntersPlay #when (be a)
    , controlledAbility a 2 (exists $ be a <> AssetReady)
        $ fightAction
        $ OrCost [exhaust a, discardCost a]
    ]

instance RunMessage Zeal where
  runMessage msg a@(Zeal attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      otherCats <- select $ oneOf [assetIs Cards.hope, assetIs Cards.augur]
      for_ otherCats $ toDiscardBy iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = attrs.ability 2
      sid <- getRandom
      discarded <- selectNone $ AssetWithId (toId attrs)
      catsInDiscard <-
        fieldMap InvestigatorDiscard (filterCards (oneOf [cardIs Cards.hope, cardIs Cards.augur])) iid
      zealCard <- field AssetCard (toId attrs)
      skillTestModifier sid source iid (BaseSkillOf #combat 5)
      when discarded $ skillTestModifier sid source sid SkillTestAutomaticallySucceeds
      pushM $ mkChooseFight sid iid source
      chooseOrRunOneM iid do
        questionLabeled "Put into play from discard"
        for_ catsInDiscard \card -> cardLabeled card do
          shuffleCardsIntoDeck iid (only zealCard)
          putCardIntoPlay iid card
        labeled "Skip" nothing
      pure a
    _ -> Zeal <$> liftRunMessage msg attrs
