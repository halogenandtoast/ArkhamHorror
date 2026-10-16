module Arkham.Homebrew.TheInvisibleMan.Treacheries.OutOfNowhere (outOfNowhere) where

import Arkham.Ability
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype OutOfNowhere = OutOfNowhere TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outOfNowhere :: TreacheryCard OutOfNowhere
outOfNowhere = treachery OutOfNowhere Cards.outOfNowhere

instance HasAbilities OutOfNowhere where
  getAbilities (OutOfNowhere a) =
    [ scenarioI18n
        $ withI18nTooltip "outOfNowhere.ambush"
        $ restricted a 1 (InThreatAreaOf You)
        $ forced enteredGriffinsLocation
    ]

instance RunMessage OutOfNowhere where
  runMessage msg t@(OutOfNowhere attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      placeInThreatArea attrs iid
      placeTokens attrs attrs #resource n
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      removeResourceOrDiscard (attrs.ability 1) attrs
      pure t
    _ -> OutOfNowhere <$> liftRunMessage msg attrs
