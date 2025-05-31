module Arkham.Location.Cards.HoldingCells (holdingCells, HoldingCells (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTest)
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Window qualified as Window

newtype HoldingCells = HoldingCells LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

holdingCells :: LocationCard HoldingCells
holdingCells = location HoldingCells Cards.holdingCells 3 (PerPlayer 1)

instance HasModifiersFor HoldingCells where
  getModifiersFor (HoldingCells a) = do
    self <- modifySelf a [CannotBeEnteredBy AnyEnemy, CannotBeFlooded]
    enemies <- modifySelect a AnyEnemy [ChangeSpawnLocation (be a) (LocationWithTitle "Sunken Grotto")]
    skillTest <-
      getSkillTest >>= \case
        Nothing -> pure mempty
        Just st -> maybeModified_ a (SkillTestTarget st.id) do
          guard $ isAbilitySource a 1 st.source
          liftGuardM $ st.investigator <=~> InvestigatorWithKey YellowKey
          pure [SkillTestAutomaticallySucceeds]
    pure $ self <> enemies <> skillTest

instance HasAbilities HoldingCells where
  getAbilities (HoldingCells attrs) =
    extendRevealed1 attrs $ skillTestAbility $ restricted attrs 1 Here actionAbility

instance RunMessage HoldingCells where
  runMessage msg l@(HoldingCells attrs) = runQueueT $ case msg of
    ForInvestigator iid (ScenarioSpecific "captured" _) -> do
      assets <- select $ assetControlledBy iid <> #hand
      for_ assets $ returnToHand iid
      enemies <- select $ enemyEngagedWith iid
      for_ enemies (disengageEnemy iid)
      place iid attrs.id
      for_ enemies enemyCheckEngagement
      checkWhen $ Window.ScenarioEvent "captured" (Just iid) (toJSON iid)
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#combat, #agility] \sType ->
          skillLabeled sType $ beginSkillTest sid iid (attrs.ability 1) iid sType (Fixed 3)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      iids <- select $ InvestigatorWithModifier (ScenarioModifier "captured")
      chooseOrRunOneM iid do
        targets iids (`forInvestigator` ScenarioSpecific "free" Null)
      pure l
    _ -> HoldingCells <$> liftRunMessage msg attrs
