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
    SearchFound iid (isTarget attrs -> True) _ (c : _) -> do
      drawCardFrom iid c iid
      let traits = toTraits c
      when (Relic `member` traits) $ gainResources iid (attrs.ability 1) 2
      when (Tome `member` traits) $ do
        chooseOneM iid $ withI18n do
          labeled' "playIt" $ playCardPayingCost iid c
          labeled' "doNotPlay" nothing
      when (Tool `member` traits) $ do
        locations <- select $ ConnectedTo (locationWithInvestigator iid) <> LocationWithAnyClues
        chooseTargetM iid locations \lid ->
          discoverAt NotInvestigate iid (attrs.ability 1) lid 1
      shuffleDeck iid
      pure a
    SearchFound _ (isTarget attrs -> True) _ _ -> do
      for_ attrs.controller shuffleDeck
      pure a
    Flip _ ScenarioSource (isTarget attrs -> True) -> do
      pure $ LucasTetlow $ attrs & flippedL .~ True & visibleL .~ False
    _ -> LucasTetlow <$> liftRunMessage msg attrs
