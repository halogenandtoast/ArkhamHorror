module Arkham.Helpers.Deck where

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Deck qualified as Deck
import Arkham.Helpers
import {-# SOURCE #-} Arkham.Helpers.Investigator (matchWho)
import Arkham.Helpers.Scenario
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher.Card qualified as Matcher
import Arkham.Message
import Arkham.Name
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Scenario.Types (Field (..))
import Arkham.Source
import Arkham.Target
import Arkham.Xp
import Control.Lens (non, _1)
import Data.Map.Strict qualified as Map

withDeck :: ([a] -> [a]) -> Deck a -> Deck a
withDeck f (Deck xs) = Deck (f xs)

withDeckM :: Functor f => ([a] -> f [a]) -> Deck a -> f (Deck a)
withDeckM f (Deck xs) = Deck <$> f xs

removeEachFromDeck :: HasCardDef a => Deck a -> [CardDef] -> Deck a
removeEachFromDeck deck removals =
  deck & withDeck \cards ->
    foldl' (\cs m -> deleteFirstMatch ((== m) . toCardDef) cs) cards removals

removeEveryFromDeck :: HasCardDef a => Deck a -> [CardDef] -> Deck a
removeEveryFromDeck deck removals = flip withDeck deck $ \cards ->
  foldl' (\cs m -> filter ((/= m) . toCardDef) cs) cards removals

{- | Split a saved campaign deck into (keep, drop) when reloading it for a scenario.
A campaign story card supersedes its copy in the saved deck. Matching is by
'canonicalCardCode' so a different printing of the same card still counts as a
duplicate -- e.g. the engine rolls the revised core Stubborn Detective (01603) as a
random basic weakness and the player hand-adds the core printing (01103) to their
decklist (#5346).
-}
partitionReloadedDeck :: [Card] -> [CardCode] -> [PlayerCard] -> ([PlayerCard], [PlayerCard])
partitionReloadedDeck storyCards invalid =
  partition \card ->
    canonicalCardCode (toCardDef card)
      `notElem` storyKeys
      && card.cardCode
      `notElem` invalid
 where
  storyKeys = map (canonicalCardCode . toCardDef) storyCards

isDeckEmpty :: (HasGame m, Deck.IsDeck deck) => deck -> m Bool
isDeckEmpty = fmap null . getDeck . Deck.toDeck

getDeck :: HasGame m => Deck.DeckSignifier -> m [Card]
getDeck = \case
  Deck.NoDeck -> pure []
  Deck.InvestigatorDeck iid -> fieldMap InvestigatorDeck (map PlayerCard . unDeck) iid
  Deck.InvestigatorDiscard iid -> fieldMap InvestigatorDiscard (map PlayerCard) iid
  Deck.EncounterDeck -> scenarioFieldMap ScenarioEncounterDeck (map EncounterCard . unDeck)
  Deck.EncounterDiscard -> scenarioFieldMap ScenarioDiscard (map EncounterCard)
  Deck.ScenarioDeckByKey k -> scenarioFieldMap ScenarioDecks (Map.findWithDefault [] k)
  Deck.InvestigatorDeckByKey iid k -> fieldMap InvestigatorDecks (Map.findWithDefault [] k) iid
  Deck.EncounterDeckByKey k -> case k of
    RegularEncounterDeck -> scenarioFieldMap ScenarioEncounterDeck (map EncounterCard . unDeck)
    other ->
      scenarioFieldMap
        ScenarioEncounterDecks
        (map EncounterCard . unDeck . view (at other . non (Deck [], []) . _1))

initDeckXp :: Monad m => Deck PlayerCard -> InvestigatorId -> Target -> m [Message]
initDeckXp deck' iid _target = pure do
  flip concatMap (toList deck') \card -> do
    case cdGrantedXp (toCardDef card) of
      Nothing -> []
      Just xp ->
        [ ReportXp
            ( XpBreakdown
                [InvestigatorGainXp iid $ XpDetail XpFromCardEffect ("$xp." <> nameToLabel card) xp]
            )
        , GainXP iid (CardIdSource card.id) xp
        , SpendXP iid xp
        ]

initDeckTrauma
  :: (MonadRandom m, HasPlayer m) => Deck PlayerCard -> InvestigatorId -> Target -> m [Message]
initDeckTrauma deck' iid target = do
  pid <- getPlayer iid
  let
    toMentalTrauma = \case
      PurchaseMentalTrauma n -> n
      _ -> 0
    toPhysicalTrauma = \case
      PurchasePhysicalTrauma n -> n
      _ -> 0
    toAnyTrauma = \case
      PurchaseAnyTrauma n -> n
      _ -> 0
    getResult (a, b, c) = (getSum a, getSum b, getSum c)

    (physicalTrauma, mentalTrauma, anyTrauma) =
      getResult
        $ foldMap
          ( \(toCardDef -> cdPurchaseTrauma -> t) -> (Sum $ toPhysicalTrauma t, Sum $ toMentalTrauma t, Sum $ toAnyTrauma t)
          )
          deck'
  chooseMsg <-
    chooseAmounts
      pid
      ("Suffer " <> tshow anyTrauma <> " total physical and/or mental trauma")
      (TotalAmountTarget anyTrauma)
      [("$physical", (0, anyTrauma)), ("$mental", (0, anyTrauma))]
      (LabeledTarget "Purchase Trauma" target)
  pure
    $ [SufferTrauma iid physicalTrauma mentalTrauma | mentalTrauma > 0 || physicalTrauma > 0]
    <> [chooseMsg | anyTrauma > 0]

deckMatch
  :: HasGame m
  => InvestigatorId
  -> Deck.DeckSignifier
  -> Matcher.DeckMatcher
  -> m Bool
deckMatch iid deckSignifier = \case
  Matcher.EncounterDeck -> pure $ deckSignifier == Deck.EncounterDeck
  Matcher.DeckOf investigatorMatcher -> case deckSignifier of
    Deck.InvestigatorDeck iid' -> matchWho iid iid' investigatorMatcher
    _ -> pure False
  Matcher.AnyDeck -> pure True
  Matcher.DeckIs deckSignifier' -> pure $ deckSignifier == deckSignifier'
  Matcher.DeckOneOf matchers' -> anyM (deckMatch iid deckSignifier) matchers'
