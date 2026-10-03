module Arkham.Homebrew.ReturnToInnsmouth.Treacheries.CallOfTheSea (callOfTheSea) where

import Arkham.Ability
import Arkham.Homebrew.ReturnToInnsmouth.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.ReturnToInnsmouth.Helpers (deepOneInvestigator)
import Arkham.Matcher
import Arkham.Message.Lifted.Move (moveToward)
import Arkham.Treachery.Import.Lifted

newtype CallOfTheSea = CallOfTheSea TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

callOfTheSea :: TreacheryCard CallOfTheSea
callOfTheSea = treachery CallOfTheSea Cards.callOfTheSea

instance HasAbilities CallOfTheSea where
  getAbilities (CallOfTheSea a) =
    [ restricted a 1 (InThreatAreaOf You) $ forced $ TurnEnds #after You
    , -- "If you have the Deep One trait, increase this ability's cost by 1 action."
      restricted a 2 (InThreatAreaOf You <> youExist (not_ deepOneInvestigator))
        $ ActionAbility mempty Nothing
        $ ActionCost 1
    , restricted a 3 (InThreatAreaOf You <> youExist deepOneInvestigator)
        $ ActionAbility mempty Nothing
        $ ActionCost 2
    ]

instance RunMessage CallOfTheSea where
  runMessage msg t@(CallOfTheSea attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      moveToward iid FullyFloodedLocation
      pure t
    UseThisAbility iid (isSource attrs -> True) n | n `elem` [2, 3] -> do
      toDiscardBy iid (attrs.ability n) attrs
      pure t
    _ -> CallOfTheSea <$> liftRunMessage msg attrs
