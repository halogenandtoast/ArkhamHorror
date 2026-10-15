module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Treacheries.MorbidAcuteness (morbidAcuteness) where

import Arkham.Ability
import Arkham.GameEnv (getPhase)
import Arkham.Helpers.Modifiers (ModifierType (..), inThreatAreaGets)
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Phase
import Arkham.Treachery.Import.Lifted

newtype MorbidAcuteness = MorbidAcuteness TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

morbidAcuteness :: TreacheryCard MorbidAcuteness
morbidAcuteness = treachery MorbidAcuteness Cards.morbidAcuteness

instance HasModifiersFor MorbidAcuteness where
  -- "You cannot draw cards during the upkeep phase."
  getModifiersFor (MorbidAcuteness a) = do
    phase <- getPhase
    when (phase == UpkeepPhase) $ inThreatAreaGets a [CannotDrawCards]

instance HasAbilities MorbidAcuteness where
  {- "[action] If you have 6 or more cards in your hand, or if you have no cards
  in your hand: Discard Morbid Acuteness." -}
  getAbilities (MorbidAcuteness a) =
    [ restricted
        a
        1
        ( OnSameLocation
            <> youExist (oneOf [HandWith (LengthIs $ atLeast 6), HandWith NoCards])
        )
        actionAbility
    ]

instance RunMessage MorbidAcuteness where
  runMessage msg t@(MorbidAcuteness attrs) = runQueueT $ case msg of
    {- "If there is a copy of Morbid Acuteness in your threat area, Morbid
    Acuteness gains surge. Otherwise, test [willpower] (3)." -}
    Revelation iid (isSource attrs -> True) -> do
      hasCopy <- selectAny $ treacheryInThreatAreaOf iid <> treacheryIs Cards.morbidAcuteness
      if hasCopy
        then gainSurge attrs
        else do
          sid <- getRandom
          revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> MorbidAcuteness <$> liftRunMessage msg attrs
