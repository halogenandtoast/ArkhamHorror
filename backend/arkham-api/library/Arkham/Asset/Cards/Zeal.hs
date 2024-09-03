module Arkham.Asset.Cards.Zeal (zeal, Zeal (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Fight
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (AssetCard)
import Arkham.Prelude
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
  runMessage msg a@(Zeal attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      otherCats <- select $ oneOf [assetIs Cards.hope, assetIs Cards.augur]
      for_ otherCats $ push . toDiscardBy iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = attrs.ability 2
      sid <- getRandom
      discarded <- selectNone $ AssetWithId (toId attrs)
      catsInDiscard <-
        fieldMap
          InvestigatorDiscard
          (filter (`cardMatch` oneOf [cardIs Cards.hope, cardIs Cards.augur]))
          iid
      player <- getPlayer iid
      zealCard <- field AssetCard (toId attrs)
      chooseFight <- toMessage <$> mkChooseFight sid iid source
      pushAll
        $ [skillTestModifier sid source iid (BaseSkillOf #combat 5)]
        <> [skillTestModifier sid source iid SkillTestAutomaticallySucceeds | discarded]
        <> [chooseFight]
        <> [ questionLabel "Put into play from discard" player
            $ ChooseOne
            $ [ CardLabel
                (toCardCode card)
                [ ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [zealCard]
                , PutCardIntoPlay iid (toCard card) Nothing NoPayment []
                ]
              | card <- catsInDiscard
              ]
            <> [Label "Skip" []]
           | notNull catsInDiscard
           ]
      pure a
    _ -> Zeal <$> runMessage msg attrs
