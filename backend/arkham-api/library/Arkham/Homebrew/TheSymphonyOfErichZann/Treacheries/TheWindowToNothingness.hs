module Arkham.Homebrew.TheSymphonyOfErichZann.Treacheries.TheWindowToNothingness (
  theWindowToNothingness,
) where

import Arkham.Ability
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.Helpers (musicTreacheriesInPlay, scenarioI18n)
import Arkham.I18n
import Arkham.Matcher hiding (InvestigatorDefeated)
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Treachery.Import.Lifted
import Arkham.Window (getBatchId)

newtype TheWindowToNothingness = TheWindowToNothingness TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWindowToNothingness :: TreacheryCard TheWindowToNothingness
theWindowToNothingness = treachery TheWindowToNothingness Cards.theWindowToNothingness

instance HasAbilities TheWindowToNothingness where
  getAbilities (TheWindowToNothingness a) =
    [ {- "Forced - When you would leave the attached location: Test [intellect]
      (X). X is the number of [[Music]] treacheries in play. If you fail, cancel
      the effects of the move." -}
      skillTestAbility
        $ forcedAbility a 1
        $ WouldMove #when You #any (LocationWithTreachery (be a)) Anywhere
    , {- "Forced - After doom is added to any card in play (including the
      agenda), each investigator at attached location is defeated. Each enemy and
      asset at attached location is discarded. Remove attached location from the
      game and attach The Window to Nothingness to any other location." -}
      mkAbility a 2 $ forced $ PlacedDoomCounter #after AnySource AnyTarget
    ]

instance RunMessage TheWindowToNothingness where
  runMessage msg t@(TheWindowToNothingness attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getBatchId -> batchId) _ -> do
      x <- length <$> musicTreacheriesInPlay
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) (BatchTarget batchId) #intellect (Fixed x)
      pure t
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      cancelMovement (attrs.ability 1) iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      case attrs.placement of
        AttachedToLocation lid -> do
          selectEach (investigatorAt lid) $ push . Msg.InvestigatorDefeated (attrs.ability 2)
          selectEach (enemyAt lid) $ toDiscard (attrs.ability 2)
          selectEach (assetAt lid) $ toDiscard (attrs.ability 2)
          others <- select $ not_ (LocationWithId lid)
          chooseOneM iid $ scenarioI18n $ scope "theWindowToNothingness" do
            targets others \other -> do
              {- Re-attach before the old location goes. `removeLocation`
              broadcasts `RemovedLocation`, and the treachery runner discards any
              treachery still directly at that location -- which would be this
              card. -}
              attachTreachery attrs other
              removeLocation lid
        _ -> pure ()
      pure t
    _ -> TheWindowToNothingness <$> liftRunMessage msg attrs
