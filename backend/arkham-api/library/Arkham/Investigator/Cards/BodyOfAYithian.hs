module Arkham.Investigator.Cards.BodyOfAYithian (BodyOfAYithian (..), YithianMetadata (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers (unDeck)
import Arkham.Helpers.Deck (withDeck)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Helpers.Window
import {-# SOURCE #-} Arkham.Investigator
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Yithian))
import Arkham.Treachery.CardDefs.TheDrownedCity qualified as Treacheries
import Data.Aeson (Result (..))

newtype YithianMetadata = YithianMetadata {originalBody :: Value}
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

newtype BodyOfAYithian = BodyOfAYithian (InvestigatorAttrs `With` YithianMetadata)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

instance IsInvestigator BodyOfAYithian where
  investigatorFromAttrs attrs = BodyOfAYithian $ attrs `with` YithianMetadata (toJSON attrs)

instance HasModifiersFor BodyOfAYithian where
  getModifiersFor (BodyOfAYithian (a `With` _)) = do
    modifySelect a (AssetControlledBy (InvestigatorWithId a.id) <> #ally) [AddTrait Yithian]

instance HasAbilities BodyOfAYithian where
  getAbilities (BodyOfAYithian a) =
    [ playerLimit PerTest
        $ selfAbility a 1 DuringYourSkillTest
        $ freeReaction
        $ CommittedCard #after You #any
    ]

instance HasChaosTokenValue BodyOfAYithian where
  getChaosTokenValue iid ElderSign (BodyOfAYithian (attrs `With` _)) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 2
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage BodyOfAYithian where
  runMessage msg i@(BodyOfAYithian (attrs `With` meta)) = runQueueT $ case msg of
    SetupInvestigator iid | iid == toId attrs -> do
      let prophecies = filterCards (cardIs Treacheries.prophecyOfTheEnd) (unDeck attrs.deck)
      for_ prophecies (removeCard . toCardId)
      -- Setting aside unique Item assets is The City of Archives' own setup instruction
      -- ("they do not exist in this time period"), not anything Body of a Yithian prints.
      -- The scenario removes them at DrawingStartingHand; we pull them from the deck
      -- first so a Permanent like Charon's Obol (1) never enters play and strands its
      -- KilledIfDefeated modifier. Unscoped, this also deleted the Relic of Ages in
      -- Shattered Aeons, leaving act 3 unadvanceable (#5929).
      inTheCityOfArchives <- any (`elem` ["04237", "53053"]) <$> selectOne TheScenario
      let
        cardFilter =
          not_
            $ oneOf
            $ cardIs Treacheries.prophecyOfTheEnd
            : [#unique <> #item <> #asset | inTheCityOfArchives]
      attrs' <- liftRunMessage msg (attrs & (deckL %~ withDeck (filterCards cardFilter)))
      pure $ BodyOfAYithian $ attrs' `with` meta
    ResolveChaosToken _ ElderSign iid | iid == toId attrs -> do
      drawCards iid ElderSign 1
      pure i
    UseCardAbility _iid (isSource attrs -> True) 1 (getCommittedCard -> card) _ -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) card DoubleSkillIcons
      pure i
    _ -> case originalInvestigator of
      Nothing -> error "the original mind of the Yithian is lost"
      Just original -> do
        original' <- liftRunMessage (Blanked msg) (overAttrs (const attrs) original)
        pure $ BodyOfAYithian $ toAttrs original' `with` meta
   where
    -- Older versions of the engine could clobber the stored snapshot (playing
    -- Transfiguration (2) re-seeded it from the Yithian's own attrs). We still know who
    -- we were from our id, so rebuild rather than bricking the game.
    originalInvestigator = case fromJSON @Investigator (originalBody meta) of
      Success original -> Just original
      Error _
        | unInvestigatorId attrs.id /= investigatorCardCode attrs ->
            Just $ lookupInvestigator attrs.id (investigatorPlayerId attrs)
      Error _ -> Nothing
