module Arkham.Investigator.Cards.BodyOfAYithian (BodyOfAYithian (..), YithianMetadata (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modified)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Helpers.Window
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Yithian))

newtype YithianMetadata = YithianMetadata {originalBody :: Value}
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

newtype BodyOfAYithian = BodyOfAYithian (InvestigatorAttrs `With` YithianMetadata)
  deriving anyclass IsInvestigator
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

instance HasModifiersFor BodyOfAYithian where
  getModifiersFor (AssetTarget aid) (BodyOfAYithian (a `With` _)) = do
    isYithian <- aid <=~> (assetControlledBy (toId a) <> #ally)
    modified a [AddTrait Yithian | isYithian]
  getModifiersFor _ _ = pure []

instance HasAbilities BodyOfAYithian where
  getAbilities (BodyOfAYithian a) =
    [ playerLimit PerTestOrAbility
        $ restrictedAbility a 1 (Self <> DuringSkillTest (YourSkillTest AnySkillTest))
        $ freeReaction
        $ CommittedCard #after You AnyCard
    ]

instance HasChaosTokenValue BodyOfAYithian where
  getChaosTokenValue iid ElderSign (BodyOfAYithian (attrs `With` _)) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 2
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage BodyOfAYithian where
  runMessage msg i@(BodyOfAYithian (attrs `With` meta)) = runQueueT $ case msg of
    ResolveChaosToken _ ElderSign iid | iid == toId attrs -> do
      drawCardsIfCan iid ElderSign 1
      pure i
    UseCardAbility _iid (isSource attrs -> True) 1 (getCommittedCard -> card) _ -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) card DoubleSkillIcons
      pure i
    _ -> BodyOfAYithian . (`with` meta) <$> liftRunMessage msg attrs
