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
import Arkham.Treachery.Cards qualified as Treacheries
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
    [ playerLimit PerTestOrAbility
        $ selfAbility a 1 DuringYourSkillTest
        $ freeReaction
        $ CommittedCard #after You AnyCard
    ]

instance HasChaosTokenValue BodyOfAYithian where
  getChaosTokenValue iid ElderSign (BodyOfAYithian (attrs `With` _)) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 2
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage BodyOfAYithian where
  runMessage msg i@(BodyOfAYithian (attrs `With` meta)) = runQueueT $ case msg of
    SetupInvestigator iid | iid == toId attrs -> do
      attrs' <- liftRunMessage msg attrs
      let prophecies = filterCards (cardIs Treacheries.prophecyOfTheEnd) (unDeck attrs.deck)
      for_ prophecies (removeCard . toCardId)
      pure
        $ BodyOfAYithian
        . (`with` meta)
        $ attrs'
        & (deckL %~ withDeck (filterCards (not_ $ cardIs Treacheries.prophecyOfTheEnd)))
    ResolveChaosToken _ ElderSign iid | iid == toId attrs -> do
      drawCards iid ElderSign 1
      pure i
    UseCardAbility _iid (isSource attrs -> True) 1 (getCommittedCard -> card) _ -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) card DoubleSkillIcons
      pure i
    _ -> case fromJSON @Investigator (originalBody meta) of
      Error _ -> error "the original mind of the Yithian is lost"
      Success original -> do
        original' <- liftRunMessage (Blanked msg) (overAttrs (const attrs) original)
        pure $ BodyOfAYithian $ toAttrs original' `with` meta
