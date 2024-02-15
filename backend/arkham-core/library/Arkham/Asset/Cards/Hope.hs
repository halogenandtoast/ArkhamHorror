module Arkham.Asset.Cards.Hope (
  hope,
  Hope (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (AssetCard)
import Arkham.Projection

newtype Hope = Hope AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hope :: AssetCard Hope
hope = asset Hope Cards.hope

instance HasAbilities Hope where
  getAbilities (Hope a) =
    [ controlledAbility a 1 (exists $ oneOf [assetIs Cards.zeal, assetIs Cards.augur])
        $ ForcedAbility
        $ AssetEntersPlay #when
        $ AssetWithId (toId a)
    , controlledAbility a 2 (exists $ AssetWithId (toId a) <> AssetReady)
        $ evadeAction
        $ OrCost [exhaust a, discardCost a]
    ]

instance RunMessage Hope where
  runMessage msg a@(Hope attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      otherCats <- select $ oneOf [assetIs Cards.zeal, assetIs Cards.augur]
      for_ otherCats $ push . toDiscardBy iid (toAbilitySource attrs 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = toAbilitySource attrs 2
      discarded <- selectNone $ AssetWithId (toId attrs)
      catsInDiscard <-
        fieldMap
          InvestigatorDiscard
          (filter (`cardMatch` oneOf [cardIs Cards.zeal, cardIs Cards.augur]))
          iid
      player <- getPlayer iid
      hopeCard <- field AssetCard (toId attrs)
      pushAll
        $ [skillTestModifier source iid (BaseSkillOf #agility 5)]
        <> [skillTestModifier source iid SkillTestAutomaticallySucceeds | discarded]
        <> [chooseEvadeEnemy iid source #agility]
        <> [ questionLabel "Put into play from discard" player
            $ ChooseOne
            $ [ CardLabel
                (toCardCode card)
                [ ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [hopeCard]
                , PutCardIntoPlay iid (toCard card) Nothing []
                ]
              | card <- catsInDiscard
              ]
            <> [Label "Skip" []]
           | notNull catsInDiscard
           ]
      pure a
    _ -> Hope <$> runMessage msg attrs
