module Arkham.Homebrew.TheSymphonyOfErichZann.Treacheries.HeardBySomething (heardBySomething) where

import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.Traits (pattern Musician)
import Arkham.I18n
import Arkham.Homebrew.TheSymphonyOfErichZann.Helpers (scenarioI18n)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Performer))
import Arkham.Treachery.Import.Lifted

newtype HeardBySomething = HeardBySomething TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heardBySomething :: TreacheryCard HeardBySomething
heardBySomething = treachery HeardBySomething Cards.heardBySomething

instance RunMessage HeardBySomething where
  runMessage msg t@(HeardBySomething attrs) = runQueueT $ case msg of
    {- "Place 1 doom on the nearest Musician enemy or Performer investigator. If
    no doom was placed by this effect, Heard by Something gains surge." -}
    Revelation iid (isSource attrs -> True) -> do
      musicians <- select $ NearestEnemyTo iid (EnemyWithTrait Musician)
      performers <- select $ InvestigatorWithTrait Performer
      case (musicians, performers) of
        ([], []) -> gainSurge attrs
        _ -> do
          let choices = map toTarget musicians <> map toTarget performers
          chooseOneM iid $ scenarioI18n $ scope "heardBySomething" do
            targets choices \target -> placeDoomOn (toSource attrs) 1 target
      pure t
    _ -> HeardBySomething <$> liftRunMessage msg attrs
