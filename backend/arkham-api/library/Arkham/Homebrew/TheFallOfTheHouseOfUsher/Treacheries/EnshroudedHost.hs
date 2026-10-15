module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Treacheries.EnshroudedHost (enshroudedHost) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.Helpers (hasAcceptedHospitality)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Treachery.Import.Lifted

newtype EnshroudedHost = EnshroudedHost TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enshroudedHost :: TreacheryCard EnshroudedHost
enshroudedHost = treachery EnshroudedHost Cards.enshroudedHost

instance HasAbilities EnshroudedHost where
  getAbilities (EnshroudedHost a) =
    [ -- "Each location gains 'Haunted - Take 1 damage.'"
      withI18n $ countVar 1 $ hauntedI "takeDamage" (proxied Anywhere a) 1
    , mkAbility a 2 actionAbility
    ]

instance RunMessage EnshroudedHost where
  runMessage msg t@(EnshroudedHost attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      place attrs NextToAgenda
      pure t
    UseThisAbility iid (isProxySource attrs -> True) 1 -> do
      assignDamage iid attrs 1
      pure t
    {- "[action]: Test [willpower] (4). If you have 'accepted the Ushers'
    hospitality,' you get +2 [willpower] for this test." -}
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      whenM (hasAcceptedHospitality iid)
        $ skillTestModifier sid (attrs.ability 2) iid (SkillModifier #willpower 2)
      beginSkillTest sid iid (attrs.ability 2) attrs #willpower (Fixed 4)
      pure t
    PassedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> do
      toDiscard (attrs.ability 2) attrs
      pure t
    _ -> EnshroudedHost <$> liftRunMessage msg attrs
