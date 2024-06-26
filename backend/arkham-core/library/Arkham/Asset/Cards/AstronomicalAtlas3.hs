module Arkham.Asset.Cards.AstronomicalAtlas3 (astronomicalAtlas3, AstronomicalAtlas3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Card
import Arkham.Helpers.Modifiers qualified as Msg
import Arkham.Matcher hiding (PlaceUnderneath)
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
        $ controlledAbility a 2 (exists $ #eligible <> CardIsBeneathAsset (be a)) (FastAbility Free)
    ]

instance RunMessage AstronomicalAtlas3 where
  runMessage msg a@(AstronomicalAtlas3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lookAt iid (attrs.ability 1) iid [(FromTopOfDeck 1, PutBack)] AnyCard (defer attrs IsNotDraw)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      focusCards attrs.cardsUnderneath $ \unfocus -> chooseOrRunOne iid $ flip map attrs.cardsUnderneath \card ->
        targetLabel
          card
          [ unfocus
          , Msg.skillTestModifier attrs card (IfSuccessfulModifier ReturnToHandAfterTest)
          , SkillTestCommitCard iid card
          ]
      pure a
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      focusCards cards \unfocus -> continue iid [unfocus, Do msg]
      pure a
    Do (SearchFound iid t@(isTarget attrs -> True) deck (card : cards)) -> do
      when (length attrs.cardsUnderneath < 5) do
        when (isNonWeakness card) $ placeUnderneath attrs [card]
        push $ Do (SearchFound iid t deck cards)
      pure a
    _ -> AstronomicalAtlas3 <$> lift (runMessage msg attrs)
