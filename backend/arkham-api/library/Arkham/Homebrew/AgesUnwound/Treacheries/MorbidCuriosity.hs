module Arkham.Homebrew.AgesUnwound.Treacheries.MorbidCuriosity (morbidCuriosity) where

import Arkham.Ability
import Arkham.Helpers.Movement (replaceMovement)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Movement (Destination (ToLocation), Movement (moveDestination))
import Arkham.Treachery.Import.Lifted

newtype MorbidCuriosity = MorbidCuriosity TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

morbidCuriosity :: TreacheryCard MorbidCuriosity
morbidCuriosity = treachery MorbidCuriosity Cards.morbidCuriosity

-- | Whichever of the two /An Earth Long Dead/ printings setup kept.
anEarthLongDead :: LocationMatcher
anEarthLongDead =
  mapOneOf locationIs [Locations.anEarthLongDead_073, Locations.anEarthLongDead_074]

{- | "Forced - When you would move to a location other than An Earth Long Dead
during your turn: Test [willpower] (2). If you fail, move to An Earth Long Dead
instead, take 1 horror and discard Morbid Curiosity."
-}
instance HasAbilities MorbidCuriosity where
  getAbilities (MorbidCuriosity a) =
    [ restricted a 1 (InThreatAreaOf You <> exists anEarthLongDead)
        $ forced
        $ WouldMove #when You #any Anywhere (not_ anEarthLongDead)
    ]

instance RunMessage MorbidCuriosity where
  runMessage msg t@(MorbidCuriosity attrs) = runQueueT $ case msg of
    -- "Revelation - Add Morbid Curiosity to your threat area."
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 2)
      pure t
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      {- "move to An Earth Long Dead *instead*": the move already in flight is
      retargeted rather than cancelled and re-queued, so the leave/enter windows
      name the destination the investigator actually reaches. -}
      selectOne anEarthLongDead >>= traverse_ \lid ->
        replaceMovement iid \m -> m {moveDestination = ToLocation lid}
      assignHorror iid (attrs.ability 1) 1
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> MorbidCuriosity <$> liftRunMessage msg attrs
