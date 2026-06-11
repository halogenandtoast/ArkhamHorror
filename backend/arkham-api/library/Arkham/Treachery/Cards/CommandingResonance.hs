module Arkham.Treachery.Cards.CommandingResonance (commandingResonance) where

import Arkham.Helpers.Location
import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Trait (Trait (Insect))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted
import Data.Bits (setBit, testBit)

newtype CommandingResonance = CommandingResonance TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

commandingResonance :: TreacheryCard CommandingResonance
commandingResonance = treachery CommandingResonance Cards.commandingResonance

instance RunMessage CommandingResonance where
  runMessage msg t@(CommandingResonance attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTestBy _iid (isSource attrs -> True) n -> do
      doStep n (DoStep 0 msg)
      pure t
    DoStep n (DoStep used msg'@(FailedThisSkillTest iid (isSource attrs -> True))) | n > 0 -> do
      hasCards <- fieldMap InvestigatorHand notNull iid
      hasInsects <- selectAny $ NearestEnemyTo iid (EnemyWithTrait Insect)
      let canDamage = not (testBit used 0)
          canDiscard = hasCards && not (testBit used 1)
          canInsect = hasInsects && not (testBit used 2)
          chooseAgain b = doStep (n - 1) (DoStep (setBit used b) msg')
      when (canDamage || canDiscard || canInsect) do
        chooseOneM iid $ withI18n do
          countVar 1 $ labeledValidate' canDamage "takeDamage" do
            assignDamage iid attrs 1
            chooseAgain 0
          countVar 1 $ labeledValidate' canDiscard "discardRandomCardsFromHand" do
            randomDiscard iid attrs
            chooseAgain 1
          labeledValidate' canInsect "nearestInsectEnemyAttacks" do
            do_ msg'
            chooseAgain 2
      pure t
    Do msg'@(FailedThisSkillTest iid (isSource attrs -> True)) -> do
      nearestInsects <- select $ NearestEnemyTo iid (EnemyWithTrait Insect)
      chooseOrRunOneM iid $ targets nearestInsects \eid -> do
        readyThis eid
        withLocationOf iid $ moveUntil eid
        forTarget eid msg'
      pure t
    ForTarget (EnemyTarget eid) (FailedThisSkillTest iid (isSource attrs -> True)) -> do
      whenMatch eid (EnemyAt $ locationWithInvestigator iid) do
        enemyEngageInvestigator eid iid
        initiateEnemyAttack eid attrs iid
      pure t
    _ -> CommandingResonance <$> liftRunMessage msg attrs
