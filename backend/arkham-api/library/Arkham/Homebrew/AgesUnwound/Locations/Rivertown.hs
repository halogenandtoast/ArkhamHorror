module Arkham.Homebrew.AgesUnwound.Locations.Rivertown (rivertown) where

import Arkham.Ability
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Traits
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement

newtype Rivertown = Rivertown LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | The one Arkham Streets location that starts revealed and in play: setup puts
it down and every investigator begins there, which is why it is also the
fallback spawn point when the Arkham Streets deck runs dry.
-}
rivertown :: LocationCard Rivertown
rivertown = symbolLabel $ location Rivertown Cards.rivertown 2 (PerPlayer 1)

{- | A [[Darkness]] treachery is either attached here or parked next to the
agenda deck, so the discard ability reaches both zones.
-}
darknessTreacheries :: LocationAttrs -> TreacheryMatcher
darknessTreacheries a =
  TreacheryWithTrait Darkness
    <> oneOf [at_ (be a), TreacheryWithPlacement NextToAgenda]

instance HasAbilities Rivertown where
  getAbilities (Rivertown a) =
    extendRevealed
      a
      [ forcedAbility a 1 $ TurnEnds #when (You <> at_ (be a))
      , restricted a 2 (Here <> exists (darknessTreacheries a)) actionAbility
      ]

instance RunMessage Rivertown where
  runMessage msg l@(Rivertown attrs) = runQueueT $ case msg of
    -- "Forced - At the end of your turn, if you are in Rivertown: Test
    -- [willpower] (2). If you fail, take 1 horror."
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 2)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignHorror iid (attrs.ability 1) 1
      pure l
    -- "[action] Discard a [[Darkness]] treachery card at this location or next
    -- to the agenda deck."
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      treacheries <- select $ darknessTreacheries attrs
      chooseTargetM iid treacheries $ toDiscardBy iid (attrs.ability 2)
      pure l
    _ -> Rivertown <$> liftRunMessage msg attrs
