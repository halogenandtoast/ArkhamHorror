module Arkham.Homebrew.TheInvisibleMan.Treacheries.MaliciousLaughter (maliciousLaughter) where

import Arkham.Ability
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype MaliciousLaughter = MaliciousLaughter TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

maliciousLaughter :: TreacheryCard MaliciousLaughter
maliciousLaughter = treachery MaliciousLaughter Cards.maliciousLaughter

instance HasAbilities MaliciousLaughter where
  getAbilities (MaliciousLaughter a) =
    [ scenarioI18n
        $ withI18nTooltip "maliciousLaughter.laughter"
        $ restricted a 1 (InThreatAreaOf You)
        $ forced enteredGriffinsLocation
    ]

instance RunMessage MaliciousLaughter where
  runMessage msg t@(MaliciousLaughter attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      placeInThreatArea attrs iid
      placeTokens attrs attrs #resource n
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      removeResourceOrDiscard (attrs.ability 1) attrs
      pure t
    _ -> MaliciousLaughter <$> liftRunMessage msg attrs
