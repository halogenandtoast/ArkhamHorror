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

rivertown :: LocationCard Rivertown
rivertown = symbolLabel $ location Rivertown Cards.rivertown 2 (PerPlayer 1)

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
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 2)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignHorror iid (attrs.ability 1) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      treacheries <- select $ darknessTreacheries attrs
      chooseTargetM iid treacheries $ toDiscardBy iid (attrs.ability 2)
      pure l
    _ -> Rivertown <$> liftRunMessage msg attrs
