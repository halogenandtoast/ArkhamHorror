module Arkham.Asset.Assets.JewelOfSarnath (jewelOfSarnath) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Deck qualified as Deck
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype JewelOfSarnath = JewelOfSarnath AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jewelOfSarnath :: AssetCard JewelOfSarnath
jewelOfSarnath = asset JewelOfSarnath Cards.jewelOfSarnath

instance HasAbilities JewelOfSarnath where
  getAbilities (JewelOfSarnath a) =
    [ mkAbility a 1 $ forced $ AssetLeavesPlay #when (be a)
    , controlled a 2 (exists $ EnemyAt YourLocation) $ FastAbility (exhaust a)
    ]

instance RunMessage JewelOfSarnath where
  runMessage msg a@(JewelOfSarnath attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      putCardIntoPlay iid attrs
      placeTokens attrs attrs #damage 3
      placeDoom attrs attrs 1
      pure a
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      shuffleIntoDeck Deck.EncounterDeck attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#willpower, #agility] \kind -> do
          skillLabeled kind $ beginSkillTest sid iid (attrs.ability 2) iid kind (Fixed 1)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      enemies <- select $ enemyAtLocationWith iid
      when (attrs.damage > 0 || attrs.doom > 0) do
        chooseOrRunOneM iid do
          targets enemies \enemy -> do
            chooseOrRunOneM iid $ withI18n do
              when (attrs.damage > 0) do
                countVar 1 $ labeled' "moveDamage" $ moveTokens (attrs.ability 2) attrs enemy #damage 1
              when (attrs.doom > 0) do
                countVar 1 $ labeled' "moveDoom" $ moveTokens (attrs.ability 2) attrs enemy #doom 1
      pure a
    _ -> JewelOfSarnath <$> liftRunMessage msg attrs
