module Arkham.Investigator.Cards.UrsulaDowns (ursulaDowns) where

import Arkham.Ability
import Arkham.Helpers.Location
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Modifier

newtype Metadata = Metadata {moveAfterTest :: Bool}
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

newtype UrsulaDowns = UrsulaDowns (InvestigatorAttrs `With` Metadata)
  deriving anyclass HasModifiersFor
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

instance IsInvestigator UrsulaDowns where
  investigatorFromAttrs = UrsulaDowns . (`with` Metadata False)

ursulaDowns :: InvestigatorCard UrsulaDowns
ursulaDowns =
  investigator (UrsulaDowns . (`with` Metadata False)) Cards.ursulaDowns
    $ Stats {health = 7, sanity = 7, willpower = 3, intellect = 4, combat = 1, agility = 4}

instance HasAbilities UrsulaDowns where
  getAbilities (UrsulaDowns (a `With` _)) =
    [ playerLimit PerRound
        $ restricted a 1 criteria
        $ freeReaction (Moves #after You AnySource Anywhere Anywhere)
    ]
   where
    criteria =
      Self
        <> oneOf
          [ exists $ PerformableAbilityBy (be a) [ActionCostModifier (-1)] <> #investigate
          , exists $ PlayableCard (UnpaidCost NoAction) #investigate <> InHandOf ForPlay (be a)
          ]

instance HasChaosTokenValue UrsulaDowns where
  getChaosTokenValue iid ElderSign (UrsulaDowns (attrs `With` _)) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage UrsulaDowns where
  runMessage msg i@(UrsulaDowns (attrs `With` metadata)) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      performActionAction iid attrs #investigate
      pure i
    ResolveChaosToken _drawnToken ElderSign iid | iid == toId attrs -> do
      pure $ UrsulaDowns $ attrs `with` Metadata True
    SkillTestEnds {} | moveAfterTest metadata -> do
      xs <- getAccessibleLocations (toId attrs) attrs
      chooseOrRunOneM attrs.id do
        labeled "Do not move to a connecting location" nothing
        when (notNull xs) do
          labeled "Move to a connecting location" do
            chooseTargetM attrs.id xs (moveTo attrs attrs.id)
      pure $ UrsulaDowns $ attrs `with` Metadata False
    ResetGame -> do
      attrs' <- liftRunMessage msg attrs
      pure $ UrsulaDowns $ attrs' `with` Metadata False
    _ -> UrsulaDowns . (`with` metadata) <$> liftRunMessage msg attrs
