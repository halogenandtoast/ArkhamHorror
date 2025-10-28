module Arkham.Treachery.Cards.Undercover (undercover) where

import Arkham.Campaigns.TheScarletKeys.Concealed
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.SkillTest.Base
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Undercover = Undercover TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

undercover :: TreacheryCard Undercover
undercover = treachery Undercover Cards.undercover

instance RunMessage Undercover where
  runMessage msg t@(Undercover attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      anyInShadows <- selectAny $ EnemyWithPlacement InTheShadows
      if anyInShadows
        then do
          sid <- getRandom
          chooseBeginSkillTestEdit sid iid attrs iid [#intellect, #agility] (Fixed 4) setIsRevelation
        else gainSurge attrs
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      locations <- selectMaxBy LocationConcealedCards length Anywhere
      decoy <- mkConcealedCard Decoy
      chooseTargetM iid locations \loc -> do
        push $ CreateConcealedCard decoy
        push $ PlaceConcealedCards iid [decoy.id] [loc]
      pure t
    _ -> Undercover <$> liftRunMessage msg attrs
