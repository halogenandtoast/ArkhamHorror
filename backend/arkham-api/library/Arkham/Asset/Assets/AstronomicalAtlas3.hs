module Arkham.Asset.Assets.AstronomicalAtlas3 (astronomicalAtlas3, AstronomicalAtlas3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Card
import Arkham.Helpers.Modifiers (ignoreCommitOneRestriction)
import Arkham.Helpers.SkillTest (getIsCommittable, withSkillTest)
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Strategy

newtype AstronomicalAtlas3 = AstronomicalAtlas3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

astronomicalAtlas3 :: AssetCard AstronomicalAtlas3
astronomicalAtlas3 = asset AstronomicalAtlas3 Cards.astronomicalAtlas3

instance HasAbilities AstronomicalAtlas3 where
  getAbilities (AstronomicalAtlas3 a) =
    [ controlledAbility a 1 (can.manipulate.deck You) $ FastAbility (exhaust a)
    , playerLimit PerTestOrAbility
        $ wantsSkillTest AnySkillTest
        $ controlledAbility a 2 (exists $ #eligible <> CardIsBeneathAsset (be a)) (FastAbility Free)
    ]

instance RunMessage AstronomicalAtlas3 where
  runMessage msg a@(AstronomicalAtlas3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lookAt iid (attrs.ability 1) iid [(FromTopOfDeck 1, PutBack)] #any (defer attrs IsNotDraw)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      committable <- ignoreCommitOneRestriction iid $ filterM (getIsCommittable iid) attrs.cardsUnderneath
      withSkillTest \sid ->
        focusCards attrs.cardsUnderneath $ \unfocus -> do
          chooseOrRunOneM iid do
            targets committable \card -> do
              push unfocus
              skillTestModifier sid attrs card (IfSuccessfulModifier ReturnToHandAfterTest)
              push $ SkillTestCommitCard iid card
      pure a
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      focusCards cards \unfocus -> continue iid [unfocus, Do msg]
      pure a
    Do (SearchFound iid t@(isTarget attrs -> True) deck (card : cards)) -> do
      when (length attrs.cardsUnderneath < 5) do
        when (isNonWeakness card) $ placeUnderneath attrs [card]
        push $ Do (SearchFound iid t deck cards)
      pure a
    _ -> AstronomicalAtlas3 <$> liftRunMessage msg attrs
