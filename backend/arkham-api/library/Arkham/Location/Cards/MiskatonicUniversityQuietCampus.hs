module Arkham.Location.Cards.MiskatonicUniversityQuietCampus (miskatonicUniversityQuietCampus) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (miskatonicUniversityQuietCampus)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Window (defaultWindows)
import Arkham.Helpers.Cost (getSpendableResources)
import Arkham.Helpers.Playable (getIsPlayableWithResources)

newtype MiskatonicUniversityQuietCampus = MiskatonicUniversityQuietCampus LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miskatonicUniversityQuietCampus :: LocationCard MiskatonicUniversityQuietCampus
miskatonicUniversityQuietCampus = location MiskatonicUniversityQuietCampus Cards.miskatonicUniversityQuietCampus 2 (PerPlayer 1)

instance HasAbilities MiskatonicUniversityQuietCampus where
  getAbilities (MiskatonicUniversityQuietCampus a) =
    extendRevealed1 a
      $ playerLimit PerGame
      $ restricted a 1 (Here <> exists (InDiscardOf You <> basic (#asset <> oneOf [#tome, #spell]))) actionAbility
  
instance RunMessage MiskatonicUniversityQuietCampus where
  runMessage msg l@(MiskatonicUniversityQuietCampus attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let ws = defaultWindows iid
      rs <- (+ 2) <$> getSpendableResources iid

      cards <-
        filterM (getIsPlayableWithResources iid GameSource rs (UnpaidCost NoAction) ws)
          =<< select (inDiscardOf iid <> basic (#asset <> oneOf [#tome, #spell]))
      chooseTargetM iid cards \c -> do
        addToHand iid (only c)
        reduceCostOf attrs c 2
        playCardPayingCost iid c
      pure l
    _ -> MiskatonicUniversityQuietCampus <$> liftRunMessage msg attrs
