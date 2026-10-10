module Arkham.Homebrew.TheMasqueOfTheRedDeath.Locations.GrandBallroom (grandBallroom) where

import Arkham.ChaosToken (pattern NegativeModifier)
import Arkham.ChaosToken.Types (ChaosTokenValue (ChaosTokenValue))
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Helpers (describedSkullEffect)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Manor))

newtype GrandBallroom = GrandBallroom LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | The starting location, and the only chamber that charges nothing to enter.
Act 1's advance flips it back to unrevealed, which is what takes the extra
connections and the resign away until act 2 buys them back.
-}
grandBallroom :: LocationCard GrandBallroom
grandBallroom = location GrandBallroom Cards.grandBallroom 2 (PerPlayer 1)

-- | The other end of "connected to each revealed [[Manor]] location".
revealedManor :: LocationAttrs -> LocationMatcher
revealedManor a = LocationWithTrait Manor <> RevealedLocation <> not_ (be a)

instance HasModifiersFor GrandBallroom where
  -- "Grand Ballroom is connected to each revealed Manor location, and vice versa."
  getModifiersFor (GrandBallroom a) = whenRevealed a do
    modifySelf a [ConnectedToWhen (be a) (revealedManor a)]
    modifySelect a (not_ $ LocationWithId a.id) [ConnectedToWhen (revealedManor a) (be a)]

instance HasAbilities GrandBallroom where
  -- "[skull]: -1 for every 2 revealed Manor locations in play." and "[action]: Resign."
  getAbilities (GrandBallroom a) =
    extendRevealed
      a
      [ describedSkullEffect 0 "-1 for every 2 revealed _Manor_ locations in play." a 1
      , locationResignAction a
      ]

instance RunMessage GrandBallroom where
  runMessage msg l@(GrandBallroom attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- Grand Ballroom is revealed whenever this resolves, so it counts itself.
      n <- selectCount $ LocationWithTrait Manor <> RevealedLocation
      withSkillTest \sid ->
        skillTestModifier sid (attrs.ability 1) sid
          $ AddChaosTokenValue (ChaosTokenValue #skull (NegativeModifier $ n `div` 2))
      pure l
    _ -> GrandBallroom <$> liftRunMessage msg attrs
