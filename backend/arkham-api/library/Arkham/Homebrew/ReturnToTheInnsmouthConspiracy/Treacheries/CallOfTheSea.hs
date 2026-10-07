module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Treacheries.CallOfTheSea (callOfTheSea) where

import Arkham.Ability
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Helpers (deepOneInvestigator)
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
    [ mkAbility a 1 (forced $ TurnEnds #after You)
        & restrict
          ( InThreatAreaOf (You <> not_ (at_ FullyFloodedLocation))
              <> exists (CanMoveCloserToLocation (a.ability 1) You FullyFloodedLocation)
          )
    , -- "If you have the Deep One trait, increase this ability's cost by 1 action."
      -- 'CostWhen', not 'CostOnlyWhen': the extra action is added for a Deep One and
      -- costs nothing for anyone else, who can still use the ability for 1 action.
      restricted a 2 (InThreatAreaOf You)
        $ actionAbilityWithCost (CostWhen (youExist deepOneInvestigator) (ActionCost 1))
    ]

instance RunMessage CallOfTheSea where
  runMessage msg t@(CallOfTheSea attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      moveToward iid FullyFloodedLocation
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> CallOfTheSea <$> liftRunMessage msg attrs
