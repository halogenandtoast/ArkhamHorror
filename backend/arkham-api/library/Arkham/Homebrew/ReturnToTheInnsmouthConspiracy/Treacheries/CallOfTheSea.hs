module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Treacheries.CallOfTheSea (callOfTheSea) where

import Arkham.Ability
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Helpers (deepOneInvestigator)
import Arkham.Matcher
import Arkham.Message.Lifted.Move (forcedMoveToward)
import Arkham.Treachery.Import.Lifted

newtype CallOfTheSea = CallOfTheSea TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

callOfTheSea :: TreacheryCard CallOfTheSea
callOfTheSea = treachery CallOfTheSea Cards.callOfTheSea

instance HasAbilities CallOfTheSea where
  getAbilities (CallOfTheSea a) =
    [ -- 'CanBeMovedCloserToLocation', not 'CanMoveCloserToLocation': the move is a
      -- mandatory instruction, so The Devil of the Depths' 2 additional actions to enter an
      -- Ocean location are not paid and cannot keep the ability from firing at end of turn,
      -- when there are no actions left to spend.
      mkAbility a 1 (forced $ TurnEnds #after You)
        & restrict
          ( InThreatAreaOf (You <> not_ (at_ FullyFloodedLocation))
              <> exists (CanBeMovedCloserToLocation (a.ability 1) You FullyFloodedLocation)
          )
    , -- "If you have the Deep One trait, increase this ability's cost by 1 action."
      -- 'CostWhen', not 'CostOnlyWhen': the extra action is added for a Deep One and
      -- costs nothing for anyone else, who can still use the ability for 1 action.
      restricted a 2 OnSameLocation
        $ actionAbilityWithCost (CostWhen (youExist deepOneInvestigator) (ActionCost 1))
    ]

instance RunMessage CallOfTheSea where
  runMessage msg t@(CallOfTheSea attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      forcedMoveToward iid FullyFloodedLocation
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> CallOfTheSea <$> liftRunMessage msg attrs
