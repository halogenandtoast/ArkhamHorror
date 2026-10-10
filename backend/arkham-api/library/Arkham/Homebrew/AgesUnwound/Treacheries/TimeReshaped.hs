module Arkham.Homebrew.AgesUnwound.Treacheries.TimeReshaped (timeReshaped) where

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
-}
instance RunMessage TimeReshaped where
  runMessage msg t@(TimeReshaped attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      chooseOneM iid $ campaignI18n do
        labeled "timeReshaped.loseActionForSkill" do
          loseStandardActions iid attrs 1
          roundModifier attrs iid (AnySkillValue 1)
        labeled "timeReshaped.gainActionForSkill" do
          gainActions iid attrs 1
          roundModifier attrs iid (AnySkillValue (-1))
      pure t
    _ -> TimeReshaped <$> liftRunMessage msg attrs
