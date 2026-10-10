module Arkham.Homebrew.AgesUnwound.Locations.DaysThatNeverWere_191b (
  daysThatNeverWere_191b,
) where

import Arkham.Ability
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.TimeRunsOut.Helpers
import Arkham.I18n
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype DaysThatNeverWere_191b = DaysThatNeverWere_191b LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "__Revelation__ - Put Days That Never Were into play."

The reverse of @:ages-unwound:191@; act 4 flips it up from beneath the agenda
deck and placing it is resolving it.
-}
daysThatNeverWere_191b :: LocationCard DaysThatNeverWere_191b
daysThatNeverWere_191b =
  symbolLabel
    $ locationWith DaysThatNeverWere_191b Cards.daysThatNeverWere_191b 4 (PerPlayer 2)
    $ revealedL
    .~ True

{- | "__Forced__ - At the end of the round, if there are any clues on this
location: Place 1 doom on the current agenda. Each investigator at this location
must either take 2 direct horror or exile a card in their hand of level 1 or
higher."
-}
instance HasAbilities DaysThatNeverWere_191b where
  getAbilities (DaysThatNeverWere_191b a) =
    extendRevealed1 a
      $ restricted a 1 (thisExists a LocationWithAnyClues)
      $ forced
      $ RoundEnds #when

instance RunMessage DaysThatNeverWere_191b where
  runMessage msg l@(DaysThatNeverWere_191b attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoomOnAgenda 1
      selectEach (investigatorAt attrs.id) \iid -> do
        exileable <- select $ inHandOf NotForPlay iid <> basic (NonWeakness <> levelOneOrHigher)
        chooseOrRunOneM iid $ timeRunsOutI18n $ scope "daysThatNeverWere" do
          countVar 2 $ labeled "takeDirectHorror" $ directHorror iid (attrs.ability 1) 2
          labeledValidate (notNull exileable) "exileACard"
            $ chooseOneM iid
            $ for_ exileable \card -> targeting card (exile card)
      pure l
    _ -> DaysThatNeverWere_191b <$> liftRunMessage msg attrs
