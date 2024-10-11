module Arkham.Asset.Assets.Augur (augur, Augur (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Investigate
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (AssetCard)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Projection

newtype Augur = Augur AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

augur :: AssetCard Augur
augur = asset Augur Cards.augur

instance HasAbilities Augur where
  getAbilities (Augur a) =
    [ controlledAbility a 1 (exists $ oneOf [assetIs Cards.zeal, assetIs Cards.hope])
        $ forced
        $ AssetEntersPlay #when (be a)
    , controlledAbility a 2 (exists $ AssetWithId (toId a) <> AssetReady)
        $ investigateAction
        $ OrCost [exhaust a, discardCost a]
    ]

instance RunMessage Augur where
  runMessage msg a@(Augur attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      otherCats <- select $ oneOf [assetIs Cards.zeal, assetIs Cards.hope]
      for_ otherCats $ toDiscardBy iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = attrs.ability 2
      discarded <- selectNone $ AssetWithId (toId attrs)
      catsInDiscard <-
        fieldMap InvestigatorDiscard (filterCards (oneOf [cardIs Cards.zeal, cardIs Cards.hope])) iid
      augurCard <- field AssetCard (toId attrs)
      sid <- getRandom
      skillTestModifier sid source iid (BaseSkillOf #intellect 5)
      when discarded $ skillTestModifier sid source sid SkillTestAutomaticallySucceeds
      pushM $ mkInvestigate sid iid source
      chooseOrRunOneM iid do
        questionLabeled "Put into play from discard"
        for_ catsInDiscard \card -> cardLabeled card do
          shuffleCardsIntoDeck iid (only augurCard)
          putCardIntoPlay iid card
        labeled "Skip" nothing
      pure a
    _ -> Augur <$> liftRunMessage msg attrs
