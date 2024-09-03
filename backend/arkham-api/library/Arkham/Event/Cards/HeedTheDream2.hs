module Arkham.Event.Cards.HeedTheDream2 (heedTheDream2, HeedTheDream2 (..)) where

import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype HeedTheDream2 = HeedTheDream2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heedTheDream2 :: EventCard HeedTheDream2
heedTheDream2 = event HeedTheDream2 Cards.heedTheDream2

instance RunMessage HeedTheDream2 where
  runMessage msg e@(HeedTheDream2 attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      investigators <- select $ affectsOthers Anyone
      cards <- for investigators $ fieldMap InvestigatorDeck (take 3 . unDeck)

      for_ (concat cards) (push . ObtainCard . toCard)

      let
        go _ [] acc = acc
        go 0 _ acc = acc
        go n ps acc = go (n - 1) (map (drop 1) ps) (acc <> [concatMap (take 1) ps])
        piles = go (3 :: Int) cards []

      let handleCard c = case c.owner of
            Just iid' -> InvestigatorDrewPlayerCard iid' c
            _ -> error "missing owner?"

      let handleRest c = case c.owner of
            Just iid' ->
              if toCard c `cardMatch` WeaknessCard
                then [ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid') [toCard c]]
                else []
            _ -> error "missing owner?"

      chooseOne
        iid
        [ CardPile
          (map (\c -> PileCard c.id c.owner) pile)
          (map handleCard pile <> concatMap handleRest (concat rest))
        | (pile, rest) <- eachWithRest piles
        ]

      pure e
    _ -> HeedTheDream2 <$> liftRunMessage msg attrs
