module Arkham.Treachery.Cards.BurdensOfThePast (burdensOfThePast) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BurdensOfThePast = BurdensOfThePast TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burdensOfThePast :: TreacheryCard BurdensOfThePast
burdensOfThePast = treachery BurdensOfThePast Cards.burdensOfThePast

instance RunMessage BurdensOfThePast where
  runMessage msg t@(BurdensOfThePast attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      hasUnfinishedBusiness <-
        selectAny $ StoryWithTitle "Unfinished Business" <> StoryWithPlacement (InThreatArea iid)
      if hasUnfinishedBusiness
        then do
          abilities <-
            select
              $ AbilityOnStory (StoryWithTitle "Unfinished Business" <> StoryWithPlacement (InThreatArea iid))
              <> AbilityIsForcedAbility
          when (notNull abilities) do
            chooseOneAtATimeM iid do
              for_ abilities \ab -> abilityLabeled iid ab nothing
        else gainSurge attrs
      pure t
    _ -> BurdensOfThePast <$> liftRunMessage msg attrs
