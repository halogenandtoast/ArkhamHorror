module Arkham.Asset.Cards.OccultLexicon3 (occultLexicon3, OccultLexicon3) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Deck
import Arkham.Event.Cards qualified as Events
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype OccultLexicon3 = OccultLexicon3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

occultLexicon3 :: AssetCard OccultLexicon3
occultLexicon3 = asset OccultLexicon3 Cards.occultLexicon3

instance HasAbilities OccultLexicon3 where
  getAbilities (OccultLexicon3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ freeReaction
        $ Matcher.PlayCard #when You
        $ BasicCardMatch
        $ cardIs Events.bloodRite
    ]

getCard :: [Window] -> Card
getCard = \case
  ((windowType -> Window.PlayCard _ card) : _) -> card
  _ -> error "impossible"

instance RunMessage OccultLexicon3 where
  runMessage msg a@(OccultLexicon3 attrs) = case msg of
    InvestigatorPlayAsset iid aid | aid == assetId attrs -> do
      handBloodRite <- PlayerCard <$> genPlayerCard Events.bloodRite
      deckBloodRites <- replicateM 2 (genCard Events.bloodRite)
      canShuffleDeck <- getCanShuffleDeck iid
      pushAll
        $ addToHand iid handBloodRite
        : [ShuffleCardsIntoDeck (InvestigatorDeck iid) deckBloodRites | canShuffleDeck]
      OccultLexicon3 <$> runMessage msg attrs
    UseCardAbility iid (isSource attrs -> True) 1 (getCard -> card) _ -> do
      push
        $ chooseOne
          iid
          [ Label
              "Change each \"2\" to a \"3\""
              [eventModifier (toAbilitySource attrs 1) (toCardId card) (MetaModifier $ object ["use3" .= True])]
          , Label
              "Shuffle it into your deck instead of discarding it"
              [eventModifier (toAbilitySource attrs 1) (toCardId card) (SetAfterPlay ShuffleThisBackIntoDeck)]
          ]
      pure a
    _ -> OccultLexicon3 <$> runMessage msg attrs
