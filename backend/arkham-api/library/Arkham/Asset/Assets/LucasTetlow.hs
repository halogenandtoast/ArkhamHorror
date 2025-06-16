module Arkham.Asset.Assets.LucasTetlow (
  lucasTetlow,
  LucasTetlow(..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Trait

newtype LucasTetlow = LucasTetlow AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lucasTetlow :: AssetCard LucasTetlow
lucasTetlow = allyWith LucasTetlow Cards.lucasTetlow (3, 1) noSlots

instance HasAbilities LucasTetlow where
  getAbilities (LucasTetlow a) =
    [ restricted a 1 ControlsThis
        $ ReactionAbility (DiscoveringLastClue #after Anyone YourLocation) (exhaust a)
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
    SearchFound iid (isTarget attrs -> True) _ (PlayerCard pc : _) -> do
      push $ InvestigatorDrewPlayerCardFrom iid pc (Just Deck.InvestigatorDeck)
      when (Relic `member` toTraits pc) $ gainResources iid (attrs.ability 1) 2
      when (Tome `member` toTraits pc) $ do
        windows' <- defaultWindows iid
        player <- getPlayer iid
        chooseOne player
          [ Label "Play it" [PayCardCost iid pc windows']
          , Label "Do not play" []
          ]
      when (Tool `member` toTraits pc) $ do
        locations <- select $ ConnectedLocation <> LocationWithAnyClues
        chooseOne iid $ targetLabels locations \lid ->
          discoverClues iid lid (attrs.ability 1) 1
      push $ ShuffleDeck iid InvestigatorDeck
      pure a
    SearchFound _ (isTarget attrs -> True) _ _ -> do
      push $ ShuffleDeck (toController attrs) InvestigatorDeck
      pure a
    _ -> LucasTetlow <$> liftRunMessage msg attrs
