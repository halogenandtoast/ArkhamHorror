module Arkham.Story.Cards.PersonalEntaglement (personalEntaglement) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype PersonalEntaglement = PersonalEntaglement StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

personalEntaglement :: StoryCard PersonalEntaglement
personalEntaglement = persistStory $ story PersonalEntaglement Cards.personalEntaglement

instance HasAbilities PersonalEntaglement where
  getAbilities (PersonalEntaglement a) =
    [ mkAbility a 1 $ forced $ PhaseEnds #when #enemy
    , restricted a 2 OnSameLocation $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) "Owner's Office")
    ]

instance RunMessage PersonalEntaglement where
  runMessage msg s@(PersonalEntaglement attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      ownersOffice <- selectJust $ LocationWithTitle "Owner's Office"
      pure $ PersonalEntaglement $ attrs & placementL .~ AtLocation ownersOffice
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      anyEnemies <- selectAny $ EnemyAt "Owner's Office"
      placeTokens (attrs.ability 1) attrs #horror $ if anyEnemies then 2 else 1
      doStep 1 msg
      pure s
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      removeFromGame attrs
      pure s
    DoStep 1 (UseThisAbility _ (isSource attrs -> True) 1) -> do
      when (attrs.token #horror >= 6) do
        lead <- getLead
        sufferMentalTrauma lead 1
        eachInvestigator \iid -> directHorror iid (attrs.ability 1) 2
        removeFromGame attrs
      pure s
    _ -> PersonalEntaglement <$> liftRunMessage msg attrs
