module Arkham.Treachery.Cards.BurdensOfThePast (
  burdensOfThePast,
  BurdensOfThePast (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype BurdensOfThePast = BurdensOfThePast TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burdensOfThePast :: TreacheryCard BurdensOfThePast
burdensOfThePast = treachery BurdensOfThePast Cards.burdensOfThePast

instance RunMessage BurdensOfThePast where
  runMessage msg t@(BurdensOfThePast attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      hasUnfinishedBusiness <-
        selectAny $ StoryWithTitle "Unfinished Business" <> StoryWithPlacement (InThreatArea iid)
      if hasUnfinishedBusiness
        then do
          abilities <-
            selectList $
              AbilityOnStory (StoryWithTitle "Unfinished Business" <> StoryWithPlacement (InThreatArea iid))
                <> AbilityIsForcedAbility
          when (notNull abilities) $
            push $
              chooseOneAtATime iid [AbilityLabel iid ability [] [] | ability <- abilities]
        else push $ GainSurge (toSource attrs) (toTarget attrs)
      pure t
    _ -> BurdensOfThePast <$> runMessage msg attrs
