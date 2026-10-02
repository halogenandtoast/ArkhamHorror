module Arkham.Homebrew.CircusExMortis.Acts.AgeOldVisions (ageOldVisions) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.Key
import Arkham.Homebrew.CircusExMortis.Traits (pattern Destiny)
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)
import Arkham.Message.Lifted.Move (enemyMoveToMatch)

newtype AgeOldVisions = AgeOldVisions ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ageOldVisions :: ActCard AgeOldVisions
ageOldVisions = act (1, A) AgeOldVisions Cards.ageOldVisions Nothing

instance HasAbilities AgeOldVisions where
  getAbilities = actAbilities1 \x ->
    -- "Objective - If there are 1[per_investigator] [[Destiny]] story cards in the
    -- victory display and 1 or more investigators are at Silent Clearing, you may
    -- advance." One destiny is dealt per investigator, so the printed count can
    -- never be exceeded and 'AtLeast' is the whole condition. "You may" makes this
    -- the investigators' choice, not an automatic advance.
    restricted
      x
      1
      ( ExtendedCardCount
          (AtLeast $ PerPlayer 1)
          (VictoryDisplayCardMatch $ basic $ CardWithTrait Destiny <> #story)
          <> exists (InvestigatorAt $ locationIs Locations.silentClearing)
      )
      $ Objective
        freeTrigger_

instance RunMessage AgeOldVisions where
  runMessage msg a@(AgeOldVisions attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    -- Back, Confirmation Bias: "Exhaust Shub-Niggurath, remove all damage from it,
    -- and move it to Silent Clearing." Horror is left alone -- it is Shub-Niggurath's
    -- own brood timer, not damage.
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      selectEach (enemyIs Enemies.shubNiggurath) \shub -> do
        exhaustThis shub
        healAllDamage attrs shub
        enemyMoveToMatch attrs shub (locationIs Locations.silentClearing)
      -- "Remove the act and agenda from the game and advance to the set-aside The
      -- Prophecy Fulfilled; it is both the current act and the current agenda." The
      -- Prophecy cards are implemented as agendas only, so advancing the agenda deck
      -- to it leaves the act deck empty.
      push $ RemoveCompletedActFromGame (actDeckId attrs) attrs.id
      advanceToAgendaA attrs Agendas.theProphecyFulfilled
      record TheProphecyWasFulfilled
      pure a
    _ -> AgeOldVisions <$> liftRunMessage msg attrs
