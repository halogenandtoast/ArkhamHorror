module Arkham.Act.Cards.CrossingTheThreshold (crossingTheThreshold) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Matcher
import Arkham.Trait

newtype Metadata = Metadata {advancingInvestigator :: Maybe InvestigatorId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype CrossingTheThreshold = CrossingTheThreshold (ActAttrs `With` Metadata)
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crossingTheThreshold :: ActCard CrossingTheThreshold
crossingTheThreshold =
  act (1, A) (CrossingTheThreshold . (`with` Metadata Nothing)) Cards.crossingTheThreshold Nothing

instance HasAbilities CrossingTheThreshold where
  getAbilities (CrossingTheThreshold (a `With` _)) =
    [ mkAbility a 1 $ Objective $ forced $ Explored #after You Anywhere $ SuccessfulExplore Anywhere
    | onSide A a
    ]

instance RunMessage CrossingTheThreshold where
  runMessage msg a@(CrossingTheThreshold (attrs `With` metadata)) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure . CrossingTheThreshold $ attrs `with` Metadata (Just iid)
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      let iid = fromJustNote "no advancing investigator" $ advancingInvestigator metadata
      sid <- getRandom
      beginSkillTest sid iid attrs iid #willpower (Fixed 4)
      advanceActDeck attrs
      pure a
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      searchCollectionForRandom iid (toSource attrs) $ BasicWeaknessCard <> hasAnyTrait [Madness, Injury]
      pure a
    RequestedPlayerCard iid (isSource attrs -> True) (Just card) _ -> do
      addCampaignCardToDeck iid ShuffleIn card
      pure a
    _ -> CrossingTheThreshold . (`with` metadata) <$> liftRunMessage msg attrs
