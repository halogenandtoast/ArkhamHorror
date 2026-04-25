module Arkham.Location.Cards.MiskatonicUniversityQuietCampus (miskatonicUniversityQuietCampus) where

import Arkham.Ability
import Arkham.Card (toCardId)
import Arkham.GameValue
import Arkham.Helpers.Message qualified as Msg
import Arkham.Location.Cards qualified as Cards (miskatonicUniversityQuietCampus)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Window (defaultWindows)

newtype MiskatonicUniversityQuietCampus = MiskatonicUniversityQuietCampus LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miskatonicUniversityQuietCampus :: LocationCard MiskatonicUniversityQuietCampus
miskatonicUniversityQuietCampus = location MiskatonicUniversityQuietCampus Cards.miskatonicUniversityQuietCampus 2 (PerPlayer 1)

instance HasAbilities MiskatonicUniversityQuietCampus where
  getAbilities (MiskatonicUniversityQuietCampus a) =
    extendRevealed1 a
      $ playerLimit PerGame
      $ restricted a 1 (Here <> exists (PlayableCard (UnpaidCost NoAction) $ InDiscardOf You <> basic (#asset <> oneOf [#tome, #spell]))) actionAbility
  
instance RunMessage MiskatonicUniversityQuietCampus where
  runMessage msg l@(MiskatonicUniversityQuietCampus attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cards <- select $ PlayableCard (UnpaidCost NoAction) $ InDiscardOf (InvestigatorWithId iid) <> basic (#asset <> oneOf [#tome, #spell])
      chooseOrRunOneM iid do
        targets cards \card -> do
          costModifier attrs iid (ReduceCostOf (CardWithId $ toCardId card) 2)
          push $ RemoveFromDiscard iid (toCardId card)
          push $ Msg.addToHand iid card
          push $ Msg.PayCardCost iid card (defaultWindows iid)
      pure l
    _ -> MiskatonicUniversityQuietCampus <$> liftRunMessage msg attrs
