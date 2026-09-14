module Arkham.Homebrew.DarkMatter.Treacheries.PerfectImitation (perfectImitation) where

import Arkham.Ability
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype PerfectImitation = PerfectImitation TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

perfectImitation :: TreacheryCard PerfectImitation
perfectImitation = treachery PerfectImitation Cards.perfectImitation

{- | "Revelation - Put Perfect Imitation into play in your threat area.
Forced - When you draw a Mimic enemy while you control an [[Ally]] asset:
Test [willpower] (4). If you fail, discard this card and an [[Ally]] asset you
control."
-}
instance HasAbilities PerfectImitation where
  getAbilities (PerfectImitation a) =
    [ restricted a 1 (InThreatAreaOf You <> youExist (HasMatchingAsset $ #ally <> DiscardableAsset))
        $ forced
        $ DrawCard #when You (basic $ cardIs Enemies.mimic) AnyDeck
    ]

instance RunMessage PerfectImitation where
  runMessage msg t@(PerfectImitation attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 4)
      pure t
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      chooseAndDiscardAssetMatching iid (attrs.ability 1) (#ally <> assetControlledBy iid)
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> PerfectImitation <$> liftRunMessage msg attrs
