module Arkham.Asset.Assets.LucasTetlow (lucasTetlow) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Strategy
import Arkham.Trait

newtype LucasTetlow = LucasTetlow AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lucasTetlow :: AssetCard LucasTetlow
lucasTetlow = allyWith LucasTetlow Cards.lucasTetlow (3, 1) noSlots

instance HasAbilities LucasTetlow where
  getAbilities (LucasTetlow a) =
    [ restricted a 1 ControlsThis
        $ triggered (DiscoveringLastClue #after Anyone YourLocation) (exhaust a)
    ]

instance RunMessage LucasTetlow where
  runMessage msg a@(LucasTetlow attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      search
        iid
        (attrs.ability 1)
        iid
        [fromTopOfDeck 9]
        (basic $ #asset <> withTrait Item)
        (defer attrs IsDraw)
      pure a
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      chooseTargetM iid cards \card -> do
        drawCardFrom iid card iid
        let traits = toTraits card
        when (Relic `member` traits) $ gainResources iid (attrs.ability 1) 2
        when (Tome `member` traits) $ do
          chooseOneM iid $ withI18n do
            labeled' "playIt" $ playCardPayingCost iid card
            labeled' "doNotPlay" nothing
        when (Tool `member` traits) $ do
          locations <- select $ connectedTo (locationWithInvestigator iid) <> LocationWithAnyClues
          chooseTargetM iid locations $ discoverAt NotInvestigate iid (attrs.ability 1) 1
      pure a
    SearchFound iid (isTarget attrs -> True) _ _ -> do
      withI18n $ prompt_ iid "noCardsFound"
      for_ attrs.controller shuffleDeck
      pure a
    Flip _ ScenarioSource (isTarget attrs -> True) -> do
      pure $ LucasTetlow $ attrs & flippedL .~ True & visibleL .~ False
    Flip _ _ (isTarget attrs -> True) -> do
      let flipped = not $ view flippedL attrs
      pure $ LucasTetlow $ attrs & flippedL .~ flipped & visibleL .~ True
    _ -> LucasTetlow <$> liftRunMessage msg attrs
