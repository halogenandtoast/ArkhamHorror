module Arkham.Homebrew.AgesUnwound.Treacheries.TimeReshaped (timeReshaped) where

import Arkham.Helpers.Investigator (getCanLoseActions)
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Treachery.Import.Lifted

newtype TimeReshaped = TimeReshaped TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | Surge is on the card def.
timeReshaped :: TreacheryCard TimeReshaped
timeReshaped = treachery TimeReshaped Cards.timeReshaped

{- | "Choose one:
- Lose 1 action. You get +1 skill value during skill tests until the end of the
  round.
- Gain 1 action. You get -1 skill value during skill tests until the end of the
  round."

The card prints no __Revelation__ header, but this is its revelation text.

"Lose 1 action" is 'loseActions', not 'loseStandardActions': the campaign's
/standard action/ term governs the effects that *count* actions, while losing
follows the core rule -- remaining actions go first, and an investigator left
with only additional actions loses those, whatever their limitation (FAQ 1.10).
So the option is gated on having any action at all to give up, not on having a
standard one.
-}
instance RunMessage TimeReshaped where
  runMessage msg t@(TimeReshaped attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      canLose <- getCanLoseActions iid
      chooseOneM iid $ campaignI18n do
        labeledValidate canLose "timeReshaped.loseActionForSkill" do
          loseActions iid attrs 1
          roundModifier attrs iid (AnySkillValue 1)
        labeled "timeReshaped.gainActionForSkill" do
          gainActions iid attrs 1
          roundModifier attrs iid (AnySkillValue (-1))
      pure t
    _ -> TimeReshaped <$> liftRunMessage msg attrs
