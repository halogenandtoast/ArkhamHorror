module Arkham.Homebrew.DarkMatter.Treacheries.Hopeless (hopeless) where

import Arkham.Ability
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Trait (Trait (Crew))
import Arkham.Treachery.Import.Lifted

newtype Hopeless = Hopeless TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hopeless :: TreacheryCard Hopeless
hopeless = treachery Hopeless Cards.hopeless

{- | "Revelation - Put Hopeless into play in your threat area.
Forced - When the agenda advances: Discard Hopeless and test [willpower] (3). If
you fail, take 1 horror for each [[Crew]] story asset at your location."
-}
instance HasAbilities Hopeless where
  getAbilities (Hopeless a) =
    [ skillTestAbility $ restricted a 1 (InThreatAreaOf You) $ forced $ AgendaAdvances #when AnyAgenda
    ]

instance RunMessage Hopeless where
  runMessage msg t@(Hopeless attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      n <- selectCount $ AssetWithTrait Crew <> AssetAt (locationWithInvestigator iid)
      when (n > 0) $ assignHorror iid (attrs.ability 1) n
      pure t
    _ -> Hopeless <$> liftRunMessage msg attrs
