module Arkham.Location.Cards.RitualSite (ritualSite) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Location.Cards qualified as Cards (ritualSite)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype RitualSite = RitualSite LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ritualSite :: LocationCard RitualSite
ritualSite = location RitualSite Cards.ritualSite 3 (PerPlayer 2)

instance HasAbilities RitualSite where
  getAbilities (RitualSite a) =
    extendRevealed1 a $ restricted a 1 (CluesOnThis $ LessThan $ PerPlayer 2) $ forced $ RoundEnds #when

instance RunMessage RitualSite where
  runMessage msg l@(RitualSite attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      cluesToAdd <- max 0 . subtract attrs.clues <$> perPlayer 2
      placeClues (attrs.ability 1) attrs cluesToAdd
      pure l
    _ -> RitualSite <$> liftRunMessage msg attrs
