module Arkham.Homebrew.AgesUnwound.Treacheries.RomanOutpost (romanOutpost) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (CannotLeavePlay), modifySelf)
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers (spawnRomanSoldier)
import Arkham.Homebrew.AgesUnwound.Traits (pattern Rome)
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype RomanOutpost = RomanOutpost TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

romanOutpost :: TreacheryCard RomanOutpost
romanOutpost = treachery RomanOutpost Cards.romanOutpost

-- | "Roman Outpost cannot leave play."
instance HasModifiersFor RomanOutpost where
  getModifiersFor (RomanOutpost a) = modifySelf a [CannotLeavePlay]

{- | "Forced - When you begin your turn at attached location, or enter it during
your turn: Put the top card of your deck into play in your threat area, as a
Roman Soldier enemy with 3 fight, 1 health, 3 evade, 1 damage and the
[[Humanoid]] trait."
-}
instance HasAbilities RomanOutpost where
  getAbilities (RomanOutpost a) =
    [ mkAbility a 1 $ forced $ TurnBegins #when (You <> at_ (locationWithTreachery a))
    , restricted a 2 (DuringTurn You) $ forced $ Enters #after You (locationWithTreachery a)
    ]

instance RunMessage RomanOutpost where
  runMessage msg t@(RomanOutpost attrs) = runQueueT $ case msg of
    {- "Revelation - Attach Roman Outpost to the nearest non-[[Rome]] location."
    "Nearest" is from the drawing investigator; with no non-Rome location in
    play there is nothing to attach to and the card surges. -}
    Revelation iid (isSource attrs -> True) -> do
      candidates <- select $ NearestLocationToYou (not_ $ LocationWithTrait Rome)
      case candidates of
        [] -> gainSurge attrs
        [lid] -> attachTreachery attrs lid
        lids -> chooseTargetM iid lids $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) n | n `elem` [1, 2] -> do
      spawnRomanSoldier iid
      pure t
    _ -> RomanOutpost <$> liftRunMessage msg attrs
