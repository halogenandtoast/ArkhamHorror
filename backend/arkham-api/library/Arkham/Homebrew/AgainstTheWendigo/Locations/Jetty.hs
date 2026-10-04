module Arkham.Homebrew.AgainstTheWendigo.Locations.Jetty (jetty) where

import Arkham.Ability
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Helpers
import Arkham.Homebrew.AgainstTheWendigo.Traits (pattern Guide, pattern Sarcee)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Placement

newtype Jetty = Jetty LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jetty :: LocationCard Jetty
jetty = location Jetty Cards.jetty 2 (PerPlayer 1)

instance HasAbilities Jetty where
  getAbilities (Jetty a) =
    extendRevealed a
      $ riverActions a
      <> [ civilizedResign a
         , -- "If no asset card with both Sarcee and Guide traits is in play,
           -- spend 3 resources: Sarcee Guide enters play. Take control of him."
           restricted
            a
            3
            (Here <> notExists (AssetWithTrait Sarcee <> AssetWithTrait Guide))
            (actionAbilityWithCost $ ResourceCost 3)
         ]


instance RunMessage Jetty where
  runMessage msg l@(Jetty attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      resolveWalkAlongTheRiver (attrs.ability 1) iid
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      resolveNavigate (attrs.ability 2) iid
      pure l
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      createAssetAt_ Assets.sarceeGuide (InPlayArea iid)
      pure l
    _ -> Jetty <$> liftRunMessage msg attrs
